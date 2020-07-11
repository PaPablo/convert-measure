(ns convert-measure.core
  (:require [clojure.string :refer [join]]
            [clojure.core.match :refer [match]]
            ))

;; { 1 } { kg } to { g } 
;; (lista de elementos)
;; [vector de elementos]
;; #{conjunto de elementos}
;; miMap = {:el valor :de este :map es :este otro}
;; (miMap :el)

(defn args->str
  "returns a string from the give list of args"
  [args]
  (join " " (map str args))
)

(defn dataMap->fn
  "Return appropiate function to use in conversion"
  [dataMap]
  (let [src (:src dataMap)
        dst (:dst dataMap)]
    (match [src dst]
           ["kg" "g"] (fn [q] (* q 1000.0))
           ["g" "kg"] (fn [q] (/ q 1000.0))
           ["oz" "g"] (fn [q] (* q 28.34952))
           ["g" "oz"] (fn [q] (/ q 28.34952))
           ["c" "f"] (fn [celsius] (-> celsius (* (/ 9 5.0)) (+ 32)))
           ["f" "c"] (fn [fahr] (-> fahr (- 32) (* (/ 5 9.0))))
           :else (fn [fahr] "Pigs and speed")
    )))

(defn argList->dataMap
  "Recieves a list of args and returns a map
  containing data to use in the convertion"
  [args]
  (let [strargs (args->str args)
        reg #"(?<quantity>\d+) (?<src>[a-zA-Z]+) to (?<dst>[a-zA-Z]+)"
        matches  (rest (re-find (re-matcher reg strargs))) ]
    (zipmap [:qty :src :dst] matches) 
    )
  )

(defn processInput [args]
  (let [dataMap (argList->dataMap args)
        operator (dataMap->fn dataMap)
        qty (Integer/parseInt (:qty dataMap))
        ]
      (operator qty)))

(defn -main 
  "Main function"
  [& args]
  (let [nArgs (count args)]
    (if (< nArgs 4)
      (println nArgs "You need to call me with: <quantity> <src_unit> to <dst_unit>")
      (println (processInput args))) )
  )
