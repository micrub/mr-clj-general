(ns mr-clj-general.core
  (:require [aprint.core :refer :all]
            [clojure.tools.logging :as log]))

(defmacro dlet [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(println (name '~n) ":" ~v)]))
                   (partition 2 bindings))]
     ~@body))

(defmacro def-
  "Private var definition macro"
  [item value]
    `(def ^{:private true} ~item ~value))

(defmacro #^{:tested? false} defhandler
  "A macro to define handler for compojure that match parameters automatically."
  (comment
    (defhandler signup [username password])
    (defroutes app-routes
      (POST "/signup" [] signup)))
  [name args & body]
  `(defn ~name [req#]
     (let [{:keys ~args :or {~'req req#}} (:params req#)]
       ~@body)))

(def apr aprint.core/aprint)

(defn now-timestamp []
  (quot (System/currentTimeMillis) 1000))

(defn node-instant []
  (java.util.Date.))

(defn #^{:tested? false
         :java-doc-url "https://docs.oracle.com/javase/7/docs/api/java/util/UUID.html#randomUUID()"}
  new-uuid []
  "Retrieve a type 4 (pseudo randomly generated) UUID. The UUID is generated using a cryptographically strong pseudo random number generator."
  (str (java.util.UUID/randomUUID)))

(defn #^{:tested? false}
  arg-count [f]
  {:pre [(instance? clojure.lang.AFunction f)]}
  (-> f class .getDeclaredMethods first .getParameterTypes alength))

(defn
  #^{:tested? false}
  map-fn-to-map [f m]
  {:pre [(map? m)
         (and
           (instance? clojure.lang.AFunction f)
           (= 1 (arg-count f)))]}
  (zipmap (map first m) (map f (map second m))))


;(defmacro map-macro-to-map [f m]
;{:pre
;[(map? m)
;(and
;(instance? clojure.lang.AFunction f)
;(.getParameterTypes
;(alength
;(.getDeclaredMethods (class f)))))]}
;(zipmap (map first m) (map f (map second m))))

(defn hexadecimalize #^{:tested? false}
  [byte-arr]
  (string/lower-case (apply str (map #(format "%02X" %) byte-arr))))


(defn modify-keys
  "Modify keys of map , convert them using function f"
  [f m]
  (zipmap (map f (keys m)) (vals m)))

(defn change-keys-to-keywords
  "Convert map keys to keywords"
  [map-data]
  (modify-keys keyword map-data))

(defn args-to-str
  "Contactination of args separated by space"
  [ args ]
  (clojure.string/join " " args))

(defn error
  "Logs error"
  [ & args]
  (log/error (args-to-str args)))

(defn info
  "Logs info"
  [ & args]
  (log/info (args-to-str args)))

(defn debug
  "Logs debug"
  [ & args]
  (log/debug (args-to-str args)))

(defn third
  "Get thrid vector's element"
  [vect]
  (get vect 2))

(defn indexof
  "Index of value in vector v"
  [value v]
  (.indexOf v value ))

(defn find-position
  "Find position of value in vector v"
  [v value]
  (filter
    #(when (second %) %)
    (keep-indexed vector
                  (map #(identity (= value (second %)))
                       (keep-indexed vector v)))))

(defn find-positions
  "Find position of each value in vector values , in vector v."
  [values v]
  (map first (map first (map (partial find-position v) values))))

(def contains?<-in-string [string needle]
  "Check if string contains other needle string
   http://stackoverflow.com/questions/26386766/check-if-string-contains-substring-in-clojure"
  (.contains string needle))
