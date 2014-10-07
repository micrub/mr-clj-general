(ns mr-clj-general.core
  (:require [aprint.core :refer :all] 
            [clojure.tools.logging :as log]))

(defn aprint aprint.core/aprint)

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

