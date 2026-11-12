(ns abi-clj.utils
  (:require
   [clojure.string :as str]
   [clojure.walk]))

(defn- digits?
  "True when `s` has only digits (at least one) from index `from`."
  [^String s ^long from]
  (let [n (.length s)]
    (and (< from n)
         (loop [i from]
           (or (= i n)
               (and (Character/isDigit (.charAt s (int i)))
                    (recur (inc i))))))))

(defn type-kind
  "Classifies a solidity type: :vector, :tuple, :bool, :address, :string,
  :unumber, :number, :bytes (fixed size) or :dbytes (dynamic). Returns nil
  for unsupported types."
  [^String type]
  (when type
    (case type
      "tuple"   :tuple
      "bool"    :bool
      "address" :address
      "string"  :string
      "bytes"   :dbytes
      (cond
        (.endsWith type "]")                                 :vector
        (.startsWith type "uint")                            :unumber
        (.startsWith type "int")                             :number
        (and (.startsWith type "bytes") (digits? type 5))    :bytes
        :else                                                nil))))

(defn vector-type
  "Splits an array type on its outermost dimension, returning
  [child-type length], with a nil length for dynamic arrays.
  \"uint256[3][]\" => [\"uint256[3]\" nil], \"uint256[3]\" => [\"uint256\" 3]"
  [^String type]
  (let [open (.lastIndexOf type "[")
        length (.substring type (inc open) (dec (.length type)))]
    [(.substring type 0 open)
     (when-not (.isEmpty length) (Long/parseLong length))]))

(defn item->is-dynamic?
  [{type :type :as item}]
  (case (type-kind type)
    (:string :dbytes) true
    :tuple            (boolean (some item->is-dynamic? (:components item)))
    :vector           (let [[child length] (vector-type type)]
                        (or (nil? length)
                            (item->is-dynamic? (assoc item :type child))))
    false))

(defn item->size
  "Size, in hex chars, the item takes in the head of its container."
  [{type :type :as item}]
  (case (type-kind type)
    :tuple  (if (item->is-dynamic? item)
              64
              (reduce + 0 (map item->size (:components item))))
    :vector (if (item->is-dynamic? item)
              64
              (let [[child length] (vector-type type)]
                (* length (item->size (assoc item :type child)))))
    64))

(defmulti item->human-readable :type)

(defmethod item->human-readable "function"
  [{inputs :inputs fname :name state :stateMutability outputs :outputs}]
  (str "function "
       fname
       (item->human-readable {:type "tuple" :components inputs})
       (when (and outputs (= state "view")) " view")
       (when outputs " returns ")
       (when outputs (item->human-readable {:type "tuple" :components outputs}))))

(defmethod item->human-readable "event"
  [{inputs :inputs ename :name}]
  (str "event "
       ename
       (item->human-readable {:type "tuple" :components inputs})))

(defmethod item->human-readable "tuple"
  [{components :components name :name}]
  (let [with-name? (every? (comp seq :name) components)]
    (format (if name
              "(%s) %s"
              "(%s)")
            (str/join (if with-name? ", " ",") (map #(item->human-readable %) components))
            name)))

(defmethod item->human-readable :default
  [{type :type name :name indexed :indexed}]
  (str type (when indexed " indexed") (when (seq name) " ") name))

(defn- dispatch-item-signature [item] (if (#{"function" "event"} (:type item))
                                        "function"
                                        (:type item)))

(defmulti item->signature #'dispatch-item-signature)

(defn dissoc-name [xs]
  (clojure.walk/postwalk
   (fn [x] (if (:components x)
             (assoc x :components (map #(dissoc % :name :indexed) (:components x)))
             x)) xs))

(defn- canonical-type
  "Type as it appears in a signature: tuples are expanded to their components'
  types, keeping the array dimensions. tuple[2][] => (uint256,bool)[2][]"
  [{^String type :type components :components}]
  (if (.startsWith type "tuple")
    (str "(" (str/join "," (map canonical-type components)) ")" (.substring type 5))
    type))

(defmethod item->signature "function"
  [{inputs :inputs fname :name}]
  (str fname "(" (str/join "," (map canonical-type inputs)) ")"))

(defmethod item->signature :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))
