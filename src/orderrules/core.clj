(ns orderrules.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clj-time.core :as c]
            [clj-time.format :as f]
            [clj-time.local :as l]))

(def custom-formatter (f/formatter "dd-MM-YYYY"))

(defrel needs fo neededfo)
(fact needs "clear" nil)
(fact needs "bb" "clear")
(fact needs "dtv" "clear")
(fact needs "tlf" "bb")
(fact needs "youbio" nil)

(defrel canstart fo neededfostatus) ;; neededfostatus is the neededfo from needs rel
(fact canstart "clear" "aktiv")
(fact canstart "clear" "kunsignal")
(fact canstart "youbio" nil)
(fact canstart "bb" "aktiv")
(fact canstart "bb" "kunsignal")
(fact canstart "dtv" "aktiv")
(fact canstart "dtv" "kunsignal")
(fact canstart "tlf" "aktiv")
(fact canstart "tlf" "kunsignal")

;; (defrel hwstatusok fo status)
;; (fact hwstatusok "clear" nil)
;; (fact hwstatusok "bb" :sent)
;; (fact hwstatusok "bb" :received)
;; (fact hwstatusok "tlf" :sent)
;; (fact hwstatusok "tlf" :received)

;; (defrel teknikerok fo status)
;; (fact teknikerok "clear" :done)
;; (fact teknikerok "bb" :done)
;; (fact teknikerok "tlf" :done)

(defrel hdok val)
(fact hdok true)

(defrel handlingsdato fo handling antal-dage hw tekniker)
(fact handlingsdato "clear" "opret" 0 false false)
(fact handlingsdato "clear" "opret" 10 false true)
(fact handlingsdato "clear" "skift" 0 false false)
(fact handlingsdato "clear" "skift" 10 false true)
(fact handlingsdato "clear" "opsig" :last-day-next-month false false)
(fact handlingsdato "clear" "opsig" :last-day-next-month false true)
(fact handlingsdato "bb" "opret" 0 false false)
(fact handlingsdato "bb" "opret" 10 false true)
(fact handlingsdato "bb" "opret" 10 true true)
(fact handlingsdato "bb" "opret" 3 true false)
(fact handlingsdato "bb" "skift" 0 false false)
(fact handlingsdato "bb" "skift" 10 false true)
(fact handlingsdato "bb" "skift" 10 true true)
(fact handlingsdato "bb" "skift" 3 true false)
(fact handlingsdato "bb" "opsig" 0 false false)
(fact handlingsdato "tlf" "opret" 0 false false)
(fact handlingsdato "tlf" "opret" 10 false true)
(fact handlingsdato "tlf" "opret" 10 true true)
(fact handlingsdato "tlf" "opret" 3 true false)
(fact handlingsdato "tlf" "skift" 0 false false)
(fact handlingsdato "tlf" "skift" 10 false true)
(fact handlingsdato "tlf" "skift" 10 true true)
(fact handlingsdato "tlf" "skift" 3 true false)
(fact handlingsdato "tlf" "opsig" 0 false false)
(fact handlingsdato "dtv" "opret" 0 false false)
(fact handlingsdato "dtv" "opret" 3 true false)
(fact handlingsdato "dtv" "skift" 0 false false)
(fact handlingsdato "dtv" "opsig" :first-day-next-month false false)
(fact handlingsdato "youbio" "opret" 0 false false)
(fact handlingsdato "youbio" "opsig" :first-day-next-month false false)

(defn handlingsdato-ok? [o nu]
  (let [hdt (f/parse custom-formatter (:handlingsdato o))
        odt (f/parse custom-formatter (:ordredato o))
        dt (if (c/after? hdt odt) hdt odt)
        hd (first (run 1 [q]
                        (handlingsdato (:aftaletype o) (.toLowerCase (:handling o)) q (:hw o) (:tekniker o))))
        tid (cond
             (= hd :last-day-next-month) (c/last-day-of-the-month (c/plus nu (c/months 1)))
             (= hd :first-day-next-month) (c/first-day-of-the-month (c/plus (l/local-now) (c/months 1)))
             :default (c/minus dt (c/days hd)))]
    (if (c/after? nu tid)
      (do ;(prn "HD1" tid)
        [true tid])
      (do ;(prn "HD2" tid)
          [false tid]))))

(defn provision-subscription? [o al & [dag]]
  (let [nu (if dag (f/parse custom-formatter dag) (l/local-now))]
    (= '(_0) (run 1 [q]
                 (fresh [needed status]
                        (conde
                         [(membero {:aftaletype needed :status status} al)]
                         [(== needed nil)]
                         )
                        (needs (:aftaletype o) needed)
                        (hdok (get (handlingsdato-ok? o nu) 0))
                        (canstart needed status)
                        )))))

(defrel transition aarsag tilstand nytilstand cos krediter)
(comment
  \                           tilstand
   \                |     NA     | afventer  |  kunsignal   |   kunplan    |    aktiv     | underlukning |  lukket  |
    ----------------|------------|-----------|--------------|--------------|--------------|--------------|----------|
 a opret            | afventer   |           |              |              |              |              |          |
 a skift            |            |           |              |              |   aktiv      |              |          |
 r luk              |            |  lukket   | underlukning | underlukning | underlukning |              |          |
 s provisioneret    |            | kunsignal |              |   aktiv      |              |              |          |
 a afprovisioneret  |            |           |              |              |  kunplan     |  lukket      |          |
 g genåbn           |            |           |              |              |              |  aktiv       | aktiv    |
   billaktiv        |            | kunplan   |   aktiv      |              |              |              |          |
   saldomaxluk      |            |           |              |              |  aktiv+cos-  |              |          |
   saldomaxåben     |            |           |              |              |  aktiv+cos+  |              |          |
   annuller         |            |  lukket   |   lukket     |   lukket     |   lukket     |   aktiv?     |          |)

(fact transition "opret" nil "afventer" nil false)
(fact transition "skift" "aktiv" "aktiv" nil false)
(fact transition "luk" "afventer" "lukket" nil false)
(fact transition "luk" "kunsignal" "underlukning" nil false)
(fact transition "luk" "kunplan" "underlukning" nil false)
(fact transition "luk" "aktiv" "underlukning" nil false)
(fact transition "provisioneret" "afventer" "kunsignal" nil false)
(fact transition "provisioneret" "kunplan" "aktiv" nil false)
(fact transition "afprovisioneret" "aktiv" "kunplan" nil false)
(fact transition "afprovisioneret" "underlukning" "lukket" nil false)
(fact transition "genåbn" "underlukning" "aktiv" nil false)
(fact transition "genåbn" "lukket" "aktiv" nil false)
(fact transition "saldomaxluk" "aktiv" "aktiv" "4" false)
(fact transition "saldomaxåben" "aktiv" "aktiv" "2" false)
(fact transition "annuller" "afventer" "lukket" nil false)
(fact transition "annuller" "kunsignal" "lukket" nil false)
(fact transition "annuller" "kunplan" "lukket" nil true)
(fact transition "annuller" "aktiv" "lukket" nil true)
(fact transition "annuller" "underlukning" "aktiv" nil false)

(defn trans [aarsag tilstand]
  (run 1 [q]
       (fresh [ny cos krediter]
              (== q [ny cos krediter])
              (transition aarsag tilstand ny cos krediter))))
