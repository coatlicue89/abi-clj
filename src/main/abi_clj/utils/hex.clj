(ns abi-clj.utils.hex
  (:refer-clojure :exclude [concat subs])
  (:import
   (java.nio.charset StandardCharsets)
   (org.bouncycastle.jcajce.provider.digest Keccak$Digest256)
   (org.bouncycastle.util.encoders Hex)))

(defn strip-0x
  "Returns `value` as a string without its leading 0x, if any."
  ^String [value]
  (let [s (str value)]
    (if (.startsWith s "0x")
      (.substring s 2)
      s)))

(def ^:private ^String zeros-64 (apply str (repeat 64 "0")))

(defn zeros
  "A string with `n` zeros, empty when `n` is not positive."
  ^String [n]
  (let [n (long n)]
    (cond
      (<= n 0)  ""
      (<= n 64) (.substring zeros-64 0 (int n))
      :else     (let [sb (StringBuilder. (int n))]
                  (dotimes [_ n] (.append sb \0))
                  (.toString sb)))))

(defn ->number
  [hex]
  (bigint (BigInteger. (strip-0x hex) 16)))

(defn number->hex
  [num]
  (str "0x" (.toString (biginteger (if (string? num) (BigInteger. ^String num) num)) 16)))

(defn pad
  ([value] (pad value {}))
  ([value {:keys [dir size] :or {size 32 dir :left}}]
   (let [s (strip-0x value)
         padding (zeros (- (* (long size) 2) (.length s)))]
     (if (= dir :left)
       (str "0x" padding s)
       (str "0x" s padding)))))

(defn concat
  "Concatenates collections of hex strings into a single 0x prefixed one."
  [& values]
  (let [sb (StringBuilder. "0x")
        append (fn [^StringBuilder sb v] (.append sb (strip-0x v)))]
    (doseq [value values]
      (if (string? value)
        (append sb value)
        (reduce append sb value)))
    (.toString sb)))

(defn size [value]
  (/ (.length (strip-0x value)) 2))

(defn subs
  ([s start]
   (str "0x" (clojure.core/subs (strip-0x s) start)))
  ([s start end]
   (str "0x" (clojure.core/subs (strip-0x s) start end))))

(defn- str->byte-arr ^bytes [s]
  (let [without0x (strip-0x s)]
    (Hex/decode ^String (if (even? (.length without0x))
                          without0x
                          (str "0" without0x)))))

(defn- k-dispatch [s]
  (cond (and (string? s) (.startsWith ^String s "0x")) :hex-str
        (string? s)            :str
        :else                  :bytes))

(defmulti keccak-256 k-dispatch)

(defmethod keccak-256 :hex-str
  [s]
  (keccak-256 (str->byte-arr s)))

(defmethod keccak-256 :str
  [^String s]
  (keccak-256 (.getBytes s StandardCharsets/UTF_8)))

(defmethod keccak-256 :bytes
  [^bytes input]
  (let [digest (Keccak$Digest256.)]
    (.update digest input 0 (alength input))
    (str "0x" (Hex/toHexString (.digest digest)))))
