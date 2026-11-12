(ns abi-clj.encode
  (:require
   [abi-clj.utils :as utils.abi]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.string :as str])
  (:import
   [java.math BigInteger]
   [java.nio.charset StandardCharsets]
   [org.bouncycastle.util.encoders Hex]))

(def ^:private ^BigInteger two-pow-256 (.shiftLeft BigInteger/ONE 256))

(defn- ->biginteger
  ^BigInteger [value]
  (if (string? value)
    (BigInteger. ^String value)
    (biginteger value)))

(defn- length-word
  "32 bytes word, without 0x, for a length or an offset."
  ^String [^long n]
  (let [hex (Long/toHexString n)]
    (str (utils.hex/zeros (- 64 (.length hex))) hex)))

(defn- encode-dispatch [{type :type}]
  (utils.abi/type-kind type))

(defmulti param #'encode-dispatch)

(defn- head-tail
  "Encodes `items` (abi items with their :value) one after the other. Static
  items go inline in the head; dynamic ones go to the tail, leaving in the
  head their offset from the start of the encoding. `head-size` is the size
  of the head in bytes."
  [items dynamic? ^long head-size]
  (let [head (StringBuilder. "0x")
        tail (StringBuilder.)]
    (doseq [item items]
      (let [encoded (utils.hex/strip-0x (param item))]
        (if (dynamic? item)
          (do (.append head (length-word (+ head-size (quot (.length tail) 2))))
              (.append tail encoded))
          (.append head encoded))))
    (.append head tail)
    (.toString head)))

(defmethod param :tuple
  [{value :value
    components :components}]
  (let [values (if (map? value)
                 (map (fn [item] (or (get value (keyword (:name item)))
                                     (get value (:name item))))
                      components)
                 value)
        items (map (fn [item value] (assoc item :value value)) components values)
        head-size (quot (long (reduce + 0 (map utils.abi/item->size items))) 2)]
    (head-tail items utils.abi/item->is-dynamic? head-size)))

(defmethod param :vector
  [{value :value
    type  :type
    :as item}]
  (let [[child-type length] (utils.abi/vector-type type)
        child-item (assoc item :type child-type)
        dynamic-child? (utils.abi/item->is-dynamic? child-item)
        len (count value)
        encoded (head-tail (map #(assoc child-item :value %) value)
                           (constantly dynamic-child?)
                           (* 32 len))]
    (if length
      encoded
      (str "0x" (length-word len) (utils.hex/strip-0x encoded)))))

(defmethod param :bool
  [{value :value}]
  (if value
    "0x0000000000000000000000000000000000000000000000000000000000000001"
    "0x0000000000000000000000000000000000000000000000000000000000000000"))

(defmethod param :address
  [{value :value}]
  (utils.hex/pad (str/lower-case value)))

(defmethod param :bytes
  [{value :value}]
  (utils.hex/pad value {:dir :right}))

(defmethod param :unumber
  [{value :value}]
  (utils.hex/pad (.toString (->biginteger value) 16)))

(defmethod param :number
  [{value :value}]
  (let [n (->biginteger value)]
    (utils.hex/pad (.toString (if (neg? (.signum n))
                                (.add n two-pow-256)
                                n)
                              16))))

(defn- length-prefixed
  "Encodes dynamic `hex` data (without 0x): its length in bytes followed by
  the data right padded to a multiple of 32 bytes."
  [^String hex]
  (let [n (.length hex)]
    (str "0x" (length-word (quot n 2)) hex (utils.hex/zeros (mod (- n) 64)))))

(defmethod param :dbytes
  [{value :value}]
  (length-prefixed (utils.hex/strip-0x value)))

(defmethod param :string
  [{value :value}]
  (length-prefixed (Hex/toHexString (.getBytes ^String value StandardCharsets/UTF_8))))

(defmethod param :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defmulti signature :type)

(defmethod signature "function"
  [function-abi-item]
  (-> (utils.abi/item->signature function-abi-item)
      utils.hex/keccak-256
      (subs 0 10)))

(defmethod signature "event" [event-abi-item]
  (-> (utils.abi/item->signature event-abi-item)
      utils.hex/keccak-256))

(defmethod signature :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defn function-call
  [{abi-item :abi-item args :args}]
  (utils.hex/concat [(signature abi-item)
                     (param {:type "tuple" :components (:inputs abi-item) :value args})]))
