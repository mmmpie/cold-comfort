(comment "
This program processes a directory of coldfusion files and reports warnings and errors that it finds.
It is intended to be used as part of a testing framework, like Jenkins.
- recursively read in a directory tree of coldfusion files
- process each file through the linter
- linter tokenizes the file
- builds a syntax tree
- looks for bad patterns in the syntax tree
    - patterns should be external plugins
    - a simple pattern might be a xpath like query of the ast
")


(ns cold-comfort.core
    (:import java.io.File)
    (:import java.io.FileNotFoundException))

(use '[clojure.string :only (split join)])


(defn as-file [s]
  "Return whatever we have as a java.io.File object"
  (cond (instance? File s) s   ; already a file, return unchanged
        (string? s) (File. s)  ; return java.io.File for path s
        :else (throw (FileNotFoundException. (str s)))))

(defn get-extension [file]
    (first (reverse (split (.getName file) #"\.")))
)

(defn walk [^File dir]
    "traverse a directory heirarchy and return a list of all the files found"
    (let [children (.listFiles dir)
          subdirs (filter #(re-matches #"^.(?!git).*" (.getName %)) (filter #(.isDirectory %) children))
          files (filter #(re-matches #".*\.(cfm|cfc)" (.getName %)) (filter #(.isFile %) children))]
        (concat files (mapcat walk subdirs))))

(defn re-pos [re s]
    (loop [m (re-matcher re s)
           res []]
      (if (.find m)
        (recur m (conj res [(.start m) (re-groups m)]))
        res)))

(defn to-ast [code]
    code)


(defn cfm [data]
    "Tokenize a cfm file"
    (let [re-tag-open #"<cf([^\s>]*)([^>]*)(/?>)"
          re-tag-close #"</cf([^>]+)>"
          open-tag-matches (re-pos re-tag-open data)
          close-tag-matches (re-pos re-tag-close data)]
      (println close-tag-matches))
)

(defn cfc [data]
    (cfm data)
)

(defn to-tokens
    [{:keys [name type data]}]
        (let [tokens ((ns-resolve 'cold-comfort.core (symbol type)) data)]
          (println tokens)))

(defn process-cf [file]
    (let [cf-file (slurp (.getAbsolutePath file))
          ast { :name (.getName file) :type (get-extension file) :data cf-file}]
      (to-tokens ast)))

(defn tokenize-cf [ast]
    
)
(defn -main [& [args]]
    (let [
        files (walk (as-file args))
        ast (mapcat process-cf files)
        tokens (doall (map println (sort (into #{} (mapcat process-cf files)))))]
    "done"))

