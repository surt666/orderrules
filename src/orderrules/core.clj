(ns orderrules.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :refer :all]
            [clj-time.core :as c]
            [clj-time.format :as f]
            [clj-time.local :as l]))

(def custom-formatter (f/formatter "dd-MM-YYYY"))

(db-rel needs fo1 fo2)

(db-rel canstart fo neededfostatus)

(db-rel hdok val)

(def reldb
  (db
   [needs "clear" nil]
   [needs "bb" "clear"]
   [needs "dtv" "clear"]
   [needs "tlf" "bb"]
   [needs "youbio" nil]
   [canstart "clear" "aktiv"]
   [canstart "clear" "kunsignal"]
   [canstart "youbio" nil]
   [canstart "bb" "aktiv"]
   [canstart "bb" "kunsignal"]
   [canstart "dtv" "aktiv"]
   [canstart "dtv" "kunsignal"]
   [canstart "tlf" "aktiv"]
   [canstart "tlf" "kunsignal"]
   [hdok true]))

(db-rel handlingsdato fo handling antal-dage hw tekniker)

(def datodb
  (db
   [handlingsdato "clear" "opret" 0 false false]
   [handlingsdato "clear" "opret" 10 false true]
   [handlingsdato "clear" "skift" 0 false false]
   [handlingsdato "clear" "skift" 10 false true]
   [handlingsdato "clear" "opsig" :last-day-next-month false false]
   [handlingsdato "clear" "opsig" :last-day-next-month false true]
   [handlingsdato "bb" "opret" 0 false false]
   [handlingsdato "bb" "opret" 10 false true]
   [handlingsdato "bb" "opret" 10 true true]
   [handlingsdato "bb" "opret" 3 true false]
   [handlingsdato "bb" "skift" 0 false false]
   [handlingsdato "bb" "skift" 10 false true]
   [handlingsdato "bb" "skift" 10 true true]
   [handlingsdato "bb" "skift" 3 true false]
   [handlingsdato "bb" "opsig" 0 false false]
   [handlingsdato "tlf" "opret" 0 false false]
   [handlingsdato "tlf" "opret" 10 false true]
   [handlingsdato "tlf" "opret" 10 true true]
   [handlingsdato "tlf" "opret" 3 true false]
   [handlingsdato "tlf" "skift" 0 false false]
   [handlingsdato "tlf" "skift" 10 false true]
   [handlingsdato "tlf" "skift" 10 true true]
   [handlingsdato "tlf" "skift" 3 true false]
   [handlingsdato "tlf" "opsig" 0 false false]
   [handlingsdato "dtv" "opret" 0 false false]
   [handlingsdato "dtv" "opret" 3 true false]
   [handlingsdato "dtv" "skift" 0 false false]
   [handlingsdato "dtv" "opsig" :first-day-next-month false false]
   [handlingsdato "youbio" "opret" 0 false false]
   [handlingsdato "youbio" "opsig" :first-day-next-month false false]))

(defn handlingsdato-ok? [o nu]
  (let [hdt (f/parse custom-formatter (:handlingsdato o))
        odt (f/parse custom-formatter (:ordredato o))
        dt (if (c/after? hdt odt) hdt odt)
        hd (first (with-db datodb
                    (doall
                     (run 1 [q]
                          (handlingsdato (:aftaletype o) (.toLowerCase (:handling o)) q (:hw o) (:tekniker o))))))
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
    (= '(_0) (with-db reldb
               (doall
                (run 1 [q]
                     (fresh [needed status]
                            (conde
                             [(membero {:aftaletype needed :status status} al)]
                             [(== needed nil)])
                            (needs (:aftaletype o) needed)
                            (hdok (get (handlingsdato-ok? o nu) 0))
                            (canstart needed status))))))))

(comment
  \                                                         tilstand
   \                   |     NA     | afventer  |  kunsignal   |   kunplan    |    aktiv     | underlukning |  lukket  |
    -------------------|------------|-----------|--------------|--------------|--------------|--------------|----------|
 a opret               | afventer   |           |              |              |              |              |          |
 a skift               |            |           |              |              |   aktiv      |              |          |
 r luk                 |            |  lukket   | underlukning | underlukning | underlukning |              |          |
 s provisioneret       |            | kunsignal |              |   aktiv      |              |              |          |
 a afprovisioneret     |            |           |              |              |  kunplan     |  lukket      |          |
 g genåbn              |            |           |              |              |              |  aktiv       | aktiv    |
   faktura-aktiv       |            | kunplan   |   aktiv      |              |              |              |          |
   faktura-inaktiv     |            |           |              |   lukket     |  kunsignal   |              |          |
   saldomaxluk         |            |           |              |              |  aktiv+cos-  |              |          |
   saldomaxåben        |            |           |              |              |  aktiv+cos+  |              |          |
   annuller            |            |  lukket   |   lukket     |   lukket     |   lukket     |   aktiv?     |          |
   flyt                |            |           |              |              |   aktiv      |              |          |
   skift-a-nr          |            |           |              |              |   aktiv      |              |          |
   fortryd-skift-a-nr  |            |           |              |              |   aktiv      |              |          |)

(db-rel transition aarsag tilstand nytilstand cos-restrict krediter)

(def transitiondb
  (db
   [transition "opret" nil "afventer" nil false]
   [transition "skift" "aktiv" "aktiv" nil false]
   [transition "luk" "afventer" "lukket" nil false]
   [transition "luk" "kunsignal" "underlukning" nil false]
   [transition "luk" "kunplan" "underlukning" nil false]
   [transition "luk" "aktiv" "underlukning" nil false]
   [transition "provisioneret" "afventer" "kunsignal" nil false]
   [transition "provisioneret" "kunplan" "aktiv" nil false]
   [transition "afprovisioneret" "aktiv" "kunplan" nil false]
   [transition "afprovisioneret" "underlukning" "lukket" nil false]
   [transition "genåbn" "underlukning" "aktiv" nil false]
   [transition "genåbn" "lukket" "aktiv" nil false]
   [transition "saldomaxluk" "aktiv" "aktiv" "4" false]
   [transition "saldomaxåben" "aktiv" "aktiv" "2" false]
   [transition "annuller" "afventer" "lukket" nil false]
   [transition "annuller" "kunsignal" "lukket" nil false]
   [transition "annuller" "kunplan" "lukket" nil true]
   [transition "annuller" "aktiv" "lukket" nil true]
   [transition "faktura-aktiv" "kunsignal" "aktiv" nil false]
   [transition "faktura-aktiv" "afventer" "kunplan" nil false]
   [transition "faktura-inaktiv" "aktiv" "kunsignal" nil false]
   [transition "faktura-inaktiv" "kunplan" "lukket" nil false]
   [transition "flyt" "aktiv" "aktiv" nil false]
   [transition "skift-a-nr" "aktiv" "aktiv" nil false]
   [transition "fortryd-skift-a-nr" "aktiv" "aktiv" nil false]))

(defn trans [aarsag tilstand]
  (with-db transitiondb
    (doall
     (run* [q]
          (fresh [ny cos krediter]
                 (== q [ny cos krediter])
                 (transition aarsag tilstand ny cos krediter))))))
