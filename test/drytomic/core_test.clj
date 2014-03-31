(ns drytomic.core-test
  (:require [clojure.test :refer :all]
            [drytomic.core :refer :all]
            [clojure.java.io :as io]))

(def input-file-path "test-in-file.erd")
(def output-file-path "test-out-file.dot")

(defn setup-input [str]
  "Create mock input file"
  (spit input-file-path str))

(defn teardown []
  "Delete mock + output files"
  (try  (io/delete-file input-file-path)
        (io/delete-file output-file-path)))

(deftest erd-test
  (testing "txs->uml-map works on trivial example"
    (is (= (txs->uml-map [{:db/ident :a/B}
                          {:db/ident :a/C}])
           {:a '(:a/B :a/C)})))

  (testing "txs->edge-set works on trivial example"
    (is (= (txs->edge-set [{:db/ident     :traffic/traffic-segments
                            :db/valueType :db.type/ref}
                           {:db/ident     :traffic-segment/start}])
           #{[:traffic :traffic-segment]})))


  (testing "erd-producer without a tfn (transformation fn)"
    (let [dot-str (setup-input (str  [{:db/ident     :traffic-segment/start}]))]
      (erd-producer input-file-path output-file-path)
      (is (= (slurp output-file-path)
             "//graph init\ndigraph documentor_graph{\n\n//options:\nnode [shape=record];\noverlap=false;\nratio=.618033989;\n\n\n//nodes:\n\":traffic-segment\" [shape=record, label=\"{:traffic-segment | \\n:traffic-segment/start}\"];\n\n\n//edges:\n\n}"))))

  (testing "erd-producer WITH a tfn (transformation fn)"
    (let [dot-str (setup-input (str  [[{:db/ident     :traffic-segment/start}]]))]
      (erd-producer input-file-path output-file-path first)
      (is (= (slurp output-file-path)
             "//graph init\ndigraph documentor_graph{\n\n//options:\nnode [shape=record];\noverlap=false;\nratio=.618033989;\n\n\n//nodes:\n\":traffic-segment\" [shape=record, label=\"{:traffic-segment | \\n:traffic-segment/start}\"];\n\n\n//edges:\n\n}"))))
  )
