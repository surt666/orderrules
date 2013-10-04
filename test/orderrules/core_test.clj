(ns orderrules.core-test
  (:require [expectations :refer :all]
            [orderrules.core :refer :all]))

(def o1 {:aftaletype "tlf" :handlingsdato "25-10-2013" :ordredato "01-09-2013" :handling "SKIFT" :hw true :tekniker true})

(def o2 {:ordredato "06-02-2013" :tekniker false :hw true :handlingsdato "06-02-2013" :handling "SKIFT" :aftaletype "tlf"})

(def o3 {:ordredato "02-10-2013" :tekniker false :hw true :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "tlf"})

(def o4 {:ordredato "02-10-2013" :tekniker false :hw false :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "tlf"})

(def o5 {:ordredato "12-09-2013" :tekniker true :hw false :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "bb"})

(def l1 [{:status "aktiv" :aftaletype "clear"} {:status "aktiv" :aftaletype "bb"} {:status "aktiv" :aftaletype "tlf"}])

(def l2 [{:status "aktiv" :aftaletype "clear"} {:status "aktiv" :aftaletype "bb"}])

(def l3 [{:status "aktiv" :aftaletype "clear"}])

(def l4 [{:status "aktiv" :aftaletype "clear"} {:status "afventer" :aftaletype "bb"}])

;; Kan ikke provisionere da handlingsdato er i fremtiden
(expect false (provision-subscription? {:aftaletype "tlf" :handlingsdato "25-10-2013" :ordredato "01-09-2013" :handling "SKIFT" :hw true :tekniker true} [{:status "aktiv" :aftaletype "clear"} {:status "aktiv" :aftaletype "bb"} {:status "aktiv" :aftaletype "tlf"}] "04-10-2013"))

;; Kan provisionere da handlingsdato i fortiden
(expect true (provision-subscription? {:ordredato "06-02-2013" :tekniker false :hw true :handlingsdato "06-02-2013" :handling "SKIFT" :aftaletype "tlf"} [{:status "aktiv" :aftaletype "clear"} {:status "aktiv" :aftaletype "bb"} {:status "aktiv" :aftaletype "tlf"}] "04-10-2013"))

;; Handlingsdato er 2 dage i fremtiden men pga hw kan vi provisionere 3 dage før handlingsdato
(expect true (provision-subscription? {:ordredato "02-10-2013" :tekniker false :hw true :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "tlf"} [{:status "aktiv" :aftaletype "clear"} {:status "aktiv" :aftaletype "bb"}] "04-10-2013"))

;; Handlingsdato er 2 dage i fremtiden og ingen hw eller tekniker så kan ikke provisionere
(expect false (provision-subscription? {:ordredato "02-10-2013" :tekniker false :hw false :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "tlf"} [{:status "aktiv" :aftaletype "clear"} {:status "aktiv" :aftaletype "bb"}] "04-10-2013"))

;; Handlingsdato er 13 dage i fremtiden og kan ikke provisioneres på trods af tekniker
(expect false (provision-subscription? {:ordredato "12-09-2013" :tekniker true :hw false :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "bb"} [{:status "aktiv" :aftaletype "clear"}] "24-09-2013"))

;; Handlingsdato er 10 dage i fremtiden og kan provisioneres på grund af tekniker
(expect true (provision-subscription? {:ordredato "12-09-2013" :tekniker true :hw false :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "bb"} [{:status "aktiv" :aftaletype "clear"}] "27-09-2013"))

;; Handlingsdato er 2 dage i fremtiden, der er hw, men vi kan ikke provisionere pga bb i afventer
(expect false (provision-subscription? {:ordredato "02-10-2013" :tekniker false :hw true :handlingsdato "06-10-2013" :handling "OPRET" :aftaletype "tlf"} [{:status "aktiv" :aftaletype "clear"} {:status "afventer" :aftaletype "bb"}] "04-10-2013"))
