(ns abi-clj.decode-test
  (:require [abi-clj.decode :as sut]
            [clojure.test :as t]))

(t/deftest decode-param
  (t/testing "Static"

    (t/testing "Should decode a uint"
      (t/is (= 69420N
               (sut/param {:type "uint256" :data "0x0000000000000000000000000000000000000000000000000000000000010f2c"})))
      (t/is (= 69420N
               (sut/param {:type "uint32" :data "0x0000000000000000000000000000000000000000000000000000000000010f2c"})))
      (t/is (= 32N
               (sut/param {:type "uint8" :data "0x0000000000000000000000000000000000000000000000000000000000000020"}))))

    (t/testing "Should decode an int"
      (t/is (= 69420N
               (sut/param {:type "int256" :data "0x0000000000000000000000000000000000000000000000000000000000010f2c"})))
      (t/is (= -69420N
               (sut/param {:type "int256" :data "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef0d4"})))
      (t/is (= 127N
               (sut/param {:type "int8" :data "0x000000000000000000000000000000000000000000000000000000000000007f"})))
      (t/is (= -128N
               (sut/param {:type "int8" :data "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80"})))
      (t/is (= 2147483647N
               (sut/param {:type "int32" :data "0x000000000000000000000000000000000000000000000000000000007fffffff"})))
      (t/is (= -2147483648N
               (sut/param {:type "int32" :data "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000"}))))

    (t/testing "Should decode an address"
      (t/is (= "0x14dc79964da2c08b23698b3d3cc7ca32193d9955"
               (sut/param {:type "address" :data "0x00000000000000000000000014dc79964da2c08b23698b3d3cc7ca32193d9955"}))))

    (t/testing "Should decode a bool"
      (t/is (= true
               (sut/param {:type "bool" :data "0x0000000000000000000000000000000000000000000000000000000000000001"})))
      (t/is (= false
               (sut/param {:type "bool" :data "0x0000000000000000000000000000000000000000000000000000000000000000"}))))

    (t/testing "Should decode bytes"
      (t/is (= "0x0123456789abcdef"
               (sut/param {:type "bytes8" :data "0x0123456789abcdef000000000000000000000000000000000000000000000000"})))

      (t/is (= "0x42069420694201023210101231415122"
               (sut/param {:type "bytes16" :data "0x4206942069420102321010123141512200000000000000000000000000000000"})))))

  (t/testing "Vectors"
    (t/testing "Should decode a uint[3]"
      (t/is (= [69420N 42069N 420420420N]
               (sut/param {:type "uint256[3]" :data "0x0000000000000000000000000000000000000000000000000000000000010f2c000000000000000000000000000000000000000000000000000000000000a45500000000000000000000000000000000000000000000000000000000190f1b44"}))))

    (t/testing "Should decode a int[3]"
      (t/is (= [69420N -42069N 420420420N]
               (sut/param {:type "int256[3]" :data "0x0000000000000000000000000000000000000000000000000000000000010f2cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff5bab00000000000000000000000000000000000000000000000000000000190f1b44"}))))

    (t/testing "Should decode a address[2]"
      (t/is (= ["0xc961145a54c96e3ae9baa048c4f4d6b04c13916b"
                "0xa5cc3c03994db5b0d9a5eedd10cabab0813678ac"]
               (sut/param {:type "address[2]" :data "0x000000000000000000000000c961145a54c96e3ae9baa048c4f4d6b04c13916b000000000000000000000000a5cc3c03994db5b0d9a5eedd10cabab0813678ac"}))))

    (t/testing "Should decode a bool[2]"
      (t/is (= [true false]
               (sut/param {:type "bool[2]" :data "0x00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000"}))))

    (t/testing "Should decode bytes[2]"
      (t/is (= ["0x1211121112111211" "0x1111111111111111"]
               (sut/param {:type "bytes8[2]" :data "0x12111211121112110000000000000000000000000000000000000000000000001111111111111111000000000000000000000000000000000000000000000000"}))))

    (t/testing "Should decode a uint[3][2]"
      (t/is (= [[69420N 42069N 420420420N]
                [420N, 44N, 422N]]
               (sut/param {:type "uint256[3][2]" :data "0x0000000000000000000000000000000000000000000000000000000000010f2c000000000000000000000000000000000000000000000000000000000000a45500000000000000000000000000000000000000000000000000000000190f1b4400000000000000000000000000000000000000000000000000000000000001a4000000000000000000000000000000000000000000000000000000000000002c00000000000000000000000000000000000000000000000000000000000001a6"}))))

    (t/testing "Should decode a uint[3][2][4]"
      (t/is (= [[[1N, 2N, 3N],
                 [4N, 5N, 6N]],
                [[7N, 8N, 9N],
                 [10N, 11N, 12N]],
                [[13N, 14N, 15N],
                 [16N, 17N, 18N]],
                [[19N, 20N, 21N],
                 [22N, 23N, 24N]]]
               (sut/param {:type "uint256[3][2][4]" :data "0x000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000009000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000b000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000000f000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000110000000000000000000000000000000000000000000000000000000000000012000000000000000000000000000000000000000000000000000000000000001300000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000015000000000000000000000000000000000000000000000000000000000000001600000000000000000000000000000000000000000000000000000000000000170000000000000000000000000000000000000000000000000000000000000018"})))))

  (t/testing "Struct"
    (t/testing "Should decode a struct: (uint256,bool,address)"
      (t/is (= [420N true "0xa5cc3c03994db5b0d9a5eedd10cabab0813678ac"]
               (sut/param {:type "tuple"
                           :components [{:type "uint256"}
                                        {:type "bool"}
                                        {:type "address"}]
                           :data "0x00000000000000000000000000000000000000000000000000000000000001a40000000000000000000000000000000000000000000000000000000000000001000000000000000000000000a5cc3c03994db5b0d9a5eedd10cabab0813678ac"}))))

    (t/testing "Should decode a struct: ((uint256,bool,address),(uint256,bool,address),uint8[2])"
      (t/is (= {:foo {:x 420N
                      :y true
                      :z "0xa5cc3c03994db5b0d9a5eedd10cabab0813678ac"}
                :baz {:x 69N
                      :y false
                      :z "0xc961145a54c96e3ae9baa048c4f4d6b04c13916b"}
                :x [1N 2N]}
               (sut/param {:type "tuple"
                           :components [{:components [{:name "x"
                                                       :type "uint256"}
                                                      {:name "y"
                                                       :type "bool"}
                                                      {:name "z"
                                                       :type "address"}]

                                         :name "foo"
                                         :type "tuple"}
                                        {:components [{:name "x"
                                                       :type "uint256"}
                                                      {:name "y"
                                                       :type "bool"}
                                                      {:name "z"
                                                       :type "address"}]
                                         :name "baz"
                                         :type "tuple"}
                                        {:name "x"
                                         :type "uint8[2]"}]
                           :data "0x00000000000000000000000000000000000000000000000000000000000001a40000000000000000000000000000000000000000000000000000000000000001000000000000000000000000a5cc3c03994db5b0d9a5eedd10cabab0813678ac00000000000000000000000000000000000000000000000000000000000000450000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c961145a54c96e3ae9baa048c4f4d6b04c13916b00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002"})))))

  (t/testing "Dynamic"
    (t/testing "Should decode a uint256[]"
      (t/is (= [420N 69N 22N 55N]
               (sut/param {:type "uint256[]" :data "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000001a4000000000000000000000000000000000000000000000000000000000000004500000000000000000000000000000000000000000000000000000000000000160000000000000000000000000000000000000000000000000000000000000037"}))))

    (t/testing "Should decode a tuple"
      (t/is (= {:foo "wagmi"
                :nft 10N}
               (sut/param {:type "tuple"
                           :components [{:type "uint256"
                                         :name "nft"}
                                        {:type "string"
                                         :name "foo"}]
                           :data "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000057761676d69000000000000000000000000000000000000000000000000000000"})))
      (t/is (= [{:foo "wagmi"
                 :nft 10N}]
               (sut/param {:type "tuple[]"
                           :components [{:type "uint256"
                                         :name "nft"}
                                        {:type "string"
                                         :name "foo"}]
                           :data "0x000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000057761676d69000000000000000000000000000000000000000000000000000000"}))))
    #_(t/testing "Should encode dynamic bytes"
        (t/is (= "0x00000000000000000000000000000000000000000000000000000000000000030420690000000000000000000000000000000000000000000000000000000000"
                 (sut/encode-param {:type "bytes" :data "0x042069"}))))

    #_(t/testing "Should encode string"
        (t/is (= "0x00000000000000000000000000000000000000000000000000000000000000057761676d69000000000000000000000000000000000000000000000000000000"
                 (sut/encode-param {:type "string" :data "wagmi"}))))))

(def ^:private w32  "0000000000000000000000000000000000000000000000000000000000000020")
(def ^:private w40  "0000000000000000000000000000000000000000000000000000000000000040")
(def ^:private w5   "0000000000000000000000000000000000000000000000000000000000000005")
(def ^:private w7   "0000000000000000000000000000000000000000000000000000000000000007")
(def ^:private wagmi "7761676d69000000000000000000000000000000000000000000000000000000")

(t/deftest decode-dynamic-bytes-and-strings
  (t/testing "Should decode a string < 32 bytes"
    (t/is (= "wagmi"
             (sut/param {:type "string" :data (str "0x" w32 w5 wagmi)}))))

  (t/testing "Should decode a string > 32 bytes without truncating it"
    (t/is (= "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
             (sut/param {:type "string"
                         :data (str "0x" w32
                                    "0000000000000000000000000000000000000000000000000000000000000038"
                                    "4c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73"
                                    "656374657475722061646970697363696e6720656c69742e0000000000000000")}))))

  (t/testing "Should decode an empty string"
    (t/is (= ""
             (sut/param {:type "string"
                         :data (str "0x" w32 "0000000000000000000000000000000000000000000000000000000000000000")}))))

  (t/testing "Should decode bytes < 32"
    (t/is (= "0x042069"
             (sut/param {:type "bytes"
                         :data (str "0x" w32
                                    "0000000000000000000000000000000000000000000000000000000000000003"
                                    "0420690000000000000000000000000000000000000000000000000000000000")}))))

  (t/testing "Should decode bytes > 32, keeping zero bytes"
    (t/is (= "0x70a08231000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"
             (sut/param {:type "bytes"
                         :data (str "0x" w32
                                    "0000000000000000000000000000000000000000000000000000000000000024"
                                    "70a08231000000000000000000000000d8da6bf26964af9d7eed9e03e53415d3"
                                    "7aa9604500000000000000000000000000000000000000000000000000000000")}))))

  (t/testing "Should decode bytes inside a tuple"
    (t/is (= {:n 7N :b "0x042069"}
             (sut/param {:type "tuple"
                         :components [{:type "uint256" :name "n"} {:type "bytes" :name "b"}]
                         :data (str "0x" w32 w7 w40
                                    "0000000000000000000000000000000000000000000000000000000000000003"
                                    "0420690000000000000000000000000000000000000000000000000000000000")})))))

(t/deftest decode-function-result
  (t/testing "Should decode a single output"
    (t/is (= 7N
             (sut/function-result {:abi-item {:outputs [{:type "uint256" :name "a"}]}
                                   :data (str "0x" w7)}))))

  (t/testing "Should decode multiple static outputs"
    (t/is (= {:a 7N :b 5N}
             (sut/function-result {:abi-item {:outputs [{:type "uint256" :name "a"} {:type "uint256" :name "b"}]}
                                   :data (str "0x" w7 w5)}))))

  (t/testing "Should decode multiple outputs when some are dynamic"
    (t/is (= {:a 7N :s "wagmi"}
             (sut/function-result {:abi-item {:outputs [{:type "uint256" :name "a"} {:type "string" :name "s"}]}
                                   :data (str "0x" w7 w40 w5 wagmi)})))
    (t/is (= [7N "wagmi"]
             (sut/function-result {:abi-item {:outputs [{:type "uint256"} {:type "string"}]}
                                   :data (str "0x" w7 w40 w5 wagmi)})))))

(t/deftest decode-event
  (let [topics ["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
                "0x00000000000000000000000014dc79964da2c08b23698b3d3cc7ca32193d9955"]]
    (t/testing "Should decode an event with static data"
      (t/is (= {:from "0x14dc79964da2c08b23698b3d3cc7ca32193d9955" :amount 7N}
               (sut/event {:abi-item {:type "event"
                                      :inputs [{:type "address" :name "from" :indexed true}
                                               {:type "uint256" :name "amount" :indexed false}]}
                           :event {:topics topics :data (str "0x" w7)}}))))

    (t/testing "Should decode an event with dynamic data"
      (t/is (= {:from "0x14dc79964da2c08b23698b3d3cc7ca32193d9955" :amount 7N :memo "wagmi"}
               (sut/event {:abi-item {:type "event"
                                      :inputs [{:type "address" :name "from" :indexed true}
                                               {:type "uint256" :name "amount" :indexed false}
                                               {:type "string" :name "memo" :indexed false}]}
                           :event {:topics topics :data (str "0x" w7 w40 w5 wagmi)}}))))))

(defn- words [& xs]
  (apply str "0x" (map #(if (string? %) % (format "%064x" %)) xs)))

(t/deftest decode-nested-arrays
  (t/testing "Should decode a dynamic array inside a tuple"
    (t/is (= {:a 7N :xs [1N 2N]}
             (sut/param {:type "tuple"
                         :components [{:type "uint256" :name "a"} {:type "uint256[]" :name "xs"}]
                         :data (words 0x20 7 0x40 2 1 2)}))))

  (t/testing "Should decode a uint256[][]"
    (t/is (= [[1N 2N] [3N]]
             (sut/param {:type "uint256[][]" :data (words 0x20 2 0x40 0xa0 2 1 2 1 3)}))))

  (t/testing "Should decode a fixed size array of dynamic items"
    (t/is (= ["wagmi" "wagmi"]
             (sut/param {:type "string[2]" :data (words 0x20 0x40 0x80 5 wagmi 5 wagmi)}))))

  (t/testing "Should decode what follows a fixed size array of structs"
    (t/is (= {:ps [{:x 1N :y 2N} {:x 3N :y 4N}] :z 9N}
             (sut/param {:type "tuple"
                         :components [{:type "tuple[2]"
                                       :name "ps"
                                       :components [{:type "uint256" :name "x"} {:type "uint256" :name "y"}]}
                                      {:type "uint256" :name "z"}]
                         :data (words 1 2 3 4 9)})))))

(t/deftest decode-int-limits
  (t/is (= 57896044618658097711785492504343953926634992332820282019728792003956564819967N
           (sut/param {:type "int256" :data (str "0x7" (apply str (repeat 63 "f")))})))
  (t/is (= -57896044618658097711785492504343953926634992332820282019728792003956564819968N
           (sut/param {:type "int256" :data (str "0x8" (apply str (repeat 63 "0")))}))))
