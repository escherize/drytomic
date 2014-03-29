(ns drytomic.core)

(defn- ident-key-ns [tx-map]
  (or  (some-> tx-map :db/ident namespace keyword)
       (-> tx-map :db/ident)))

(defn- ident-key-name [tx-map]
  (or  (some-> tx-map :db/ident name keyword)
       (-> tx-map :db/ident)))

;; More about graphviz settings:
;; http://www.graphviz.org/pdf/neato.1.pdf
(defn- dot-file-wrapper [graph-str & opts]
  (str "digraph documentor_graph{\n"
       "node [shape=record];\n"
       "overlap=false;\n"       ;; nodes will not overlap
       "ratio=.618033989;\n"    ;;golden ratio / 1
       "///////end options///////\n\n\n"
       graph-str
       "}"))

(defn- uml-str [uml-map]
  (let [title (-> uml-map key)
        attrs (-> uml-map val)]
    (str "\"" title "\""
         " [shape=record, label=\"{"
         title " | \\n"
         (clojure.string/join " \\n" attrs)
         "}\"];\n")))

(defn- uml-strs [uml-maps]
  (map uml-str uml-maps))

(defn- txs->edge-set [tx-maps]
  (->> (for [source tx-maps
             target tx-maps
             :when (and (= (:db/valueType source) :db.type/ref)
                        (= (-> source ident-key-name str)
                           (-> target ident-key-ns (str "s"))))]
         [(ident-key-ns source)
          (ident-key-ns target)])
       (into #{})))

(defn- edge-strs [edge-set]
  (map (fn [[source target]]
         (str "\"" source "\" -> \"" target "\";\n")) edge-set))

(defn- txs->uml-map [txs]
  (->> txs
       (group-by ident-key-ns)
       vals
       (map (fn [tx-maps]
              (let [title  (-> tx-maps first ident-key-ns)
                    idents (map :db/ident tx-maps)]
                [title idents])))
       (into {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input should be a seq of tx-maps.
;; E.g. http://docs.datomic.com/schema.html

(defn erd-producer [in-path out-path]
  (let [txs (-> in-path slurp read-string)
        umlstrs  (->> txs txs->uml-map uml-strs)
        edgestrs (->> txs txs->edge-set edge-strs)]
    (->> (str "//nodes:\n"
              (apply str umlstrs)
              "\n\n"
              "//edges:\n"
              (apply str edgestrs))
         dot-file-wrapper
         (spit out-path))))
