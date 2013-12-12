(ns orderrules.core-test
  (:require [expectations :refer :all]
            [orderrules.core :refer :all])
  (:import [org.joda.time DateTime]))

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

;; is the regex in the string
(expect #"foo" (str "boo" "foo" "ar"))

;; does the form throw an expected exception
(expect ArithmeticException (/ 12 0))

(given [x y] (expect x (in y))
       :a #{:a :b}
       {:a :b} {:a :b :c :d})

;; verify the type of the result
;(expect-focused String "foo")

(expect {:foo 1 :bar {:bar2 "test" :foo3 4}} (in {:foo 1 :cat 4 :bar {:bar2 "test" :foo3 4}}))

(given {:foo 1 :bar {:bar2 "test" :foo3 4}}
       (expect
        :foo 1
        :bar {:bar2 "test" :foo3 4}))

(given {:foo 1 :bar {:bar2 "test" :foo3 4}}
       (expect
        :foo 1
        (comp in :bar) {:bar2 "test"}
        (comp in :bar) {:foo3 4}))

(given (java.util.ArrayList.)
       (expect
        .size 0
        .isEmpty true))

(given {:foo 1 :bar [{:baz 2 :baz2 3} {:baz 4 :baz2 5} {:baz 6 :baz2 7}]}
       (expect
        :foo 1
        (comp count :bar) 3
        (comp first :bar) {:baz 2 :baz2 3}
        (comp in first :bar) {:baz 2}
        (comp :baz first :bar) 2
        (comp :baz2 #(% 2) :bar) 7))  ;; hent 3. element af bar og check baz2

(defn timestamp-event [m t]
  (assoc m :time t))

(expect-let [now (DateTime.)]
            {:time now}
            (timestamp-event {} now))

(expect empty? (list))

(expect java.util.AbstractMap java.util.HashMap)
