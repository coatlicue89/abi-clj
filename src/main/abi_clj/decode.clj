(ns abi-clj.decode
  (:require
   [abi-clj.utils :as utils.abi]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.string :as str])
  (:import
   [java.lang Integer]
   [java.math BigInteger]
   [java.nio.charset StandardCharsets]
   [org.bouncycastle.util.encoders Hex]))

(defn- remove-0x [data]
  (str/replace data #"^0x" ""))

(defn- extract-data-at
  ([data cursor] (extract-data-at data cursor 1))
  ([data cursor len] (subs (remove-0x data) cursor (+ cursor (* len 64)))))

(defn- decode-dispatch [{type :type}]
  (cond
    (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type) :vector
    (= type "tuple")                   :tuple
    (= type "bool")                    :bool
    (= type "address")                 :address
    (= type "string")                  :string
    (re-matches #"^uint.*" type)       :unumber
    (re-matches #"^int.*" type)        :number
    (re-matches #"^bytes(\d+)$" type)  :bytes
    (re-matches #"^bytes$" type)       :dbytes))

(defmulti param #'decode-dispatch)

(defn- decode-components
  "Decodes `components` laid out sequentially starting at `base` (the start of
  the tuple body, which is also what dynamic offsets are relative to). Returns
  a vector with the decoded values, in order."
  [components data base]
  (loop [cursor base components (seq components) res []]
    (if-let [component (first components)]
      (let [dynamic? (utils.abi/item->is-dynamic? component)
            decoded (param (assoc component :data data :cursor cursor :offset (if dynamic? base cursor)))]
        (recur (+ cursor (utils.abi/item->size component)) (next components) (conj res decoded)))
      res)))

(defn- name-values
  "Returns a map keyed by component name when every component is named,
  otherwise the values as a vector."
  [components values]
  (if (every? :name components)
    (into {} (map (fn [component value] [(keyword (:name component)) value]) components values))
    (vec values)))

(defn- dynamic-bytes-at
  "Hex (without 0x) of the length prefixed bytes pointed by the offset stored at `cursor`."
  [data cursor ini-offset]
  (let [offset (param {:type "uint256" :data data :cursor cursor})
        len-cursor (long (+ ini-offset (* 2 offset)))
        len (long (param {:type "uint256" :data data :cursor len-cursor}))
        data-cursor (+ len-cursor 64)]
    (subs data data-cursor (+ data-cursor (* 2 len)))))

(defmethod param :tuple
  [{data :data
    components :components
    ini-cursor :cursor
    ini-offset :offset
    :or {ini-cursor 0 ini-offset 0} :as item}]
  (let [data (remove-0x data)
        offset (if (utils.abi/item->is-dynamic? item)
                 (param {:type "uint256" :data data :cursor ini-cursor})
                 0)]
    (name-values components (decode-components components data (+ ini-offset (* 2 offset))))))

(defmethod param :vector
  [{data :data
    type :type
    ini-cursor :cursor
    ini-offset :offset
    :or {ini-cursor 0 ini-offset 0} :as item}]
  (let [data (remove-0x data)
        match (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d*\])+)(\[\d*\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        size (utils.abi/item->size (assoc item :type (str type conc-dim)))
        dynamic? (utils.abi/item->is-dynamic? item)
        [len ini-data-cursor] (if dynamic?
                                (let [offset (param {:type "uint256" :data data :cursor ini-cursor})]
                                  [(param {:type "uint256" :data data :cursor (+ ini-offset (* 2 offset))})
                                   (+ ini-cursor (* 2 offset) 64)])

                                [(-> (last match)
                                     (str/replace #"\[|\]" "")
                                     Integer/parseInt)
                                 ini-cursor])]

    (->> (iterate #(+ size %) 0)
         (take len)
         (map #(let [dynamic? (utils.abi/item->is-dynamic? (assoc item :type (str type conc-dim)))]
                 (param (assoc item
                               :type (str type conc-dim)
                               :data data
                               :cursor (+ ini-data-cursor %)
                               :offset (if dynamic?
                                         ini-data-cursor
                                         (+ ini-data-cursor %))))))
         vec)))

(defmethod param :unumber
  [{data :data cursor :cursor :or {cursor 0}}]
  (bigint (BigInteger. (extract-data-at data cursor) 16)))

(defmethod param :number
  [{data :data cursor :cursor :or {cursor 0}}]
  (let [val (BigInteger. (extract-data-at data cursor) 16)
        max (.subtract (.shiftLeft (BigInteger. (str "1")) (- (* 32 8) 1)) (BigInteger. (str 1)))]
    (bigint
     (if (< val max) val
         (.subtract val (.shiftLeft (BigInteger. (str "1")) (* 32 8)))))))

(defmethod param :address
  [{data :data cursor :cursor :or {cursor 0}}]
  (utils.hex/concat [(-> (extract-data-at data cursor)
                         (subs (- 64 40) 64))]))

(defmethod param :bool
  [{data :data cursor :cursor :or {cursor 0}}]
  (< 0 (BigInteger. (extract-data-at data cursor))))

(defmethod param :bytes
  [{data :data type :type cursor :cursor :or {cursor 0}}]
  (let [size (->> (re-matches #"^bytes(\d+)$" type)
                  second
                  Integer/parseInt)]
    (utils.hex/concat [(subs (extract-data-at data cursor) 0 (* size 2))])))

(defmethod param :string
  [{data :data cursor :cursor ini-offset :offset :or {cursor 0 ini-offset 0}}]
  (String. ^bytes (Hex/decode ^String (dynamic-bytes-at (remove-0x data) cursor ini-offset))
           StandardCharsets/UTF_8))

(defmethod param :dbytes
  [{data :data cursor :cursor ini-offset :offset :or {cursor 0 ini-offset 0}}]
  (str "0x" (dynamic-bytes-at (remove-0x data) cursor ini-offset)))

(defmethod param :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defn- hashed-when-indexed?
  "Reference types (strings, bytes, arrays and structs) are not stored in the
  topic when indexed, only their keccak hash is."
  [item]
  (or (utils.abi/item->is-dynamic? item)
      (= "tuple" (:type item))
      (str/includes? (:type item) "[")))

(defn event
  [{abi-item :abi-item event :event}]
  (let [indexed (filter :indexed (:inputs abi-item))
        not-indexed (remove :indexed (:inputs abi-item))
        from-topics (map (fn [item topic]
                           (if (hashed-when-indexed? item)
                             topic
                             (param (assoc item :data topic))))
                         indexed
                         (rest (:topics event)))
        from-data (decode-components not-indexed (remove-0x (:data event)) 0)]
    (name-values (concat indexed not-indexed) (concat from-topics from-data))))

(defn function-result
  [{abi-item :abi-item data :data}]
  (let [outputs (:outputs abi-item)]
    (if (= 1 (count outputs))
      (param (assoc (first outputs) :data data))
      (name-values outputs (decode-components outputs (remove-0x data) 0)))))
