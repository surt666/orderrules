(ns orderrules.flow
  (:require [clojure.core.async :refer :all]))

(declare opret-bb-abon-step)
(declare opret-clear-abon-step)
(declare opret-tlf-abon-step)

(def all-steps [opret-bb-abon-step opret-clear-abon-step opret-tlf-abon-step])

(defn step [c kundeid stepfn i]
  (go
   (let [val (apply stepfn [kundeid i])]
     (println "V1" val)
     (>! c val)))
  (go
   (let [val (<! c)]
     (println "V2" val)
     val)))

(defn opret-clear-abon-step [kundeid i]
  (prn "clear")
  (Thread/sleep (rand-int 1000))
  (if (< i 4) :ok :done)) ;; TODO should be able to return :not-done

(defn opret-bb-abon-step [kundeid i]
  (prn "bb")
  (Thread/sleep (rand-int 1000))
  (if (< i 3) :ok :done))

(defn opret-tlf-abon-step [kundeid i]
  (prn "tlf")
  (Thread/sleep (rand-int 1000))
  (if (< i 5) :ok :done))

(defn ordre [kundeid]
  (let [c (chan)]
    (loop [res [] i 0]
      (if (and (not= i 0) (empty? (filter #(= :ok %) res)))
        (do (prn "BLAAAA" res i)
            (close! c))
        (do (prn "TEST" res i)
            (recur (mapv <!! (map #(step c kundeid % i) all-steps)) (inc i)))))))

(defn test []
  (let [c (chan)]
    (go
     (>! c :ok))
    (<!! (go
      (let [val (<! c)]
        val)))))
