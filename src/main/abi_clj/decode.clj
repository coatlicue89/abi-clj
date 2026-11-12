(ns abi-clj.decode
  (:require
   [abi-clj.utils :as utils.abi]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.string :as str])
  (:import
   [java.math BigInteger]
   [java.nio.charset StandardCharsets]
   [org.bouncycastle.util.encoders Hex]))

(def ^:private ^BigInteger two-pow-256 (.shiftLeft BigInteger/ONE 256))

(defn- remove-0x ^String [data]
  (utils.hex/strip-0x data))

(defn- uint-at
  "The 32 bytes word starting at `cursor` (in hex chars) as an unsigned integer."
  ^BigInteger [^String data cursor]
  (let [cursor (int cursor)]
    (BigInteger. (.substring data cursor (+ cursor 64)) 16)))

(defn- long-at
  "Like `uint-at`, for lengths and offsets. Throws when it doesn't fit a long."
  ^long [data cursor]
  (.longValueExact (uint-at data cursor)))

(defn- decode-dispatch [{type :type}]
  (utils.abi/type-kind type))

(defmulti param #'decode-dispatch)

(defn- decode-components
  "Decodes `components` laid out sequentially starting at `base` (the start of
  the tuple body, which is also what dynamic offsets are relative to). Returns
  a vector with the decoded values, in order."
  [components data base]
  (loop [cursor (long base) components (seq components) res []]
    (if-let [component (first components)]
      (let [dynamic? (utils.abi/item->is-dynamic? component)
            decoded (param (assoc component :data data :cursor cursor :offset (if dynamic? base cursor)))]
        (recur (+ cursor (long (utils.abi/item->size component))) (next components) (conj res decoded)))
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
  ^String [^String data cursor ini-offset]
  (let [len-cursor (+ (long ini-offset) (* 2 (long-at data cursor)))
        len (long-at data len-cursor)
        data-cursor (+ len-cursor 64)]
    (.substring data (int data-cursor) (int (+ data-cursor (* 2 len))))))

(defmethod param :tuple
  [{data :data
    components :components
    ini-cursor :cursor
    ini-offset :offset
    :or {ini-cursor 0 ini-offset 0} :as item}]
  (let [data (remove-0x data)
        offset (if (utils.abi/item->is-dynamic? item)
                 (long-at data ini-cursor)
                 0)]
    (name-values components (decode-components components data (+ (long ini-offset) (* 2 offset))))))

(defmethod param :vector
  [{data :data
    type :type
    ini-cursor :cursor
    ini-offset :offset
    :or {ini-cursor 0 ini-offset 0} :as item}]
  (let [data (remove-0x data)
        [child-type length] (utils.abi/vector-type type)
        child-item (assoc item :type child-type :data data)
        dynamic-child? (utils.abi/item->is-dynamic? child-item)
        size (long (utils.abi/item->size child-item))
        ;; dynamic arrays live in the tail, pointed by an offset
        start (if (utils.abi/item->is-dynamic? item)
                (+ (long ini-offset) (* 2 (long-at data ini-cursor)))
                (long ini-cursor))
        ;; only arrays without a fixed length are prefixed by it
        len (long (or length (long-at data start)))
        body (if length start (+ start 64))]
    (mapv (fn [^long i]
            (let [cursor (+ body (* i size))]
              (param (assoc child-item
                            :cursor cursor
                            :offset (if dynamic-child? body cursor)))))
          (range len))))

(defmethod param :unumber
  [{data :data cursor :cursor :or {cursor 0}}]
  (bigint (uint-at (remove-0x data) cursor)))

(defmethod param :number
  [{data :data cursor :cursor :or {cursor 0}}]
  (let [val (uint-at (remove-0x data) cursor)]
    (bigint (if (.testBit val 255)
              (.subtract val two-pow-256)
              val))))

(defmethod param :address
  [{data :data cursor :cursor :or {cursor 0}}]
  (let [cursor (int cursor)]
    (str "0x" (.substring (remove-0x data) (+ cursor 24) (+ cursor 64)))))

(defmethod param :bool
  [{data :data cursor :cursor :or {cursor 0}}]
  (pos? (.signum (uint-at (remove-0x data) cursor))))

(defmethod param :bytes
  [{data :data ^String type :type cursor :cursor :or {cursor 0}}]
  (let [size (Long/parseLong (.substring type 5))
        cursor (int cursor)]
    (str "0x" (.substring (remove-0x data) cursor (+ cursor (* size 2))))))

(defmethod param :string
  [{data :data cursor :cursor ini-offset :offset :or {cursor 0 ini-offset 0}}]
  (String. ^bytes (Hex/decode (dynamic-bytes-at (remove-0x data) cursor ini-offset))
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
