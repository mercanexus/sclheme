(ns sclheme.core
  (:gen-class))


(declare funca funcb)

(defn tokenise [s]
  (filter seq (-> s (clojure.string/replace "(" " ( ")
                  (clojure.string/replace ")" " ) ")
                  (clojure.string/split #"\s+"))))

(defn map-thing [s]
  (cond (#{"(" ")"} s) s
        :else (let [num (clojure.edn/read-string s)]
                (if (number? num) num s))))

(defn count-tkn [tokens tkn]
  "Counts how many times tkn appears in tokens"
  (count (for [e tokens :when (= e tkn)] e)))


(defn get-close-paren-idx [tokens]
  "Given a list of tokens which starts with an open paren, return
the indices of all close parens in tokens"
  (let [midx (map-indexed #(vector %1 %2) tokens)]
    (for [[i e] midx :when (= e ")")] i)))

(defn balanced-parens? [tokens]
  "Balanced parens here means equal number of open and close parens"
  (let [o (count-tkn tokens "(")
        c (count-tkn tokens ")")]
    (= o c)))

(defn get-sub-tokens [tokens]
  "Given tokens which starts with a open paren return all tokens in
between this open paren and its matching close paren (exclusive)"
  (let [idxs (get-close-paren-idx tokens)
        subs (map #(vector %1 (drop 1 (take %1 tokens))) idxs)
        ntkns (apply min (for [[n sub] subs :when (balanced-parens? sub)] n))]
    (drop 1 (take ntkns tokens))))

(defn get-matching-parens [tokens]
  "Given tokens which begin with a open paren return the part of tokens which
begins with the matching close paren"
  (let [idxs (get-close-paren-idx tokens)
        subs (map #(vector %1 (drop 1 (take %1 tokens))) idxs)
        ntkns (apply min (for [[n sub] subs :when (balanced-parens? sub)] n))]
    (drop ntkns tokens)))

(defn funcb [tokens]
  "Called by funca and either returns the first token from tokens or
initiates the creation of a new vector of tokens"
  (let [token (first tokens)]
    (if (= token "(") (funca (get-sub-tokens tokens) [])
        token)))

(defn funca
  "Given a S-expression in tokenised form return an abstract syntax
tree representation of the S-expression. Note this function does
not check if the sexp is properly formed and will likely fall over
if it receives bad input"
  ([tokens-] (funca tokens- nil))
  ([tokens- ast-]
   (loop [tokens tokens-
          ast ast-]
     (if (nil? (seq tokens)) ast
         (let [t (first tokens)
               tokens% (if (= t "(") (next (get-matching-parens tokens))
                           (next tokens))
               ast% (if (nil? ast) (funcb tokens)
                        (conj ast (funcb tokens)))]
           (recur tokens% ast%))))))

;;; TODO:
;;; - tokeniser
;;;

;;; test cases follow

(def tokes1 ["(" "the" "quick" "(" "(" "brown" ")" "fox" ")" "jumped" ")"])

(def tokes2 ["(" "a"  "(" "d" "(" "(" "b" ")" "e" ")" ")" "c" ")"])

(def tokes3 ["(" "(" 'a   'b  ")"   'c   "("   "("  'd  ")"   'e  ")"  ")"])

(def tokes4 ["(" "(" 'apple ")" "(" 'banana ")" 'orange  "(" 'mango ")"  'guava  'kiwi-fruit ")"])

(defn tcase1 []
  (funca tokes1))

(defn tcase2 []
  (funca tokes2))

(defn tcase3 []
  (funca tokes3))

(defn tcase4 []
  (funca tokes4))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
  
