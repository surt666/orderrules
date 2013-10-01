(ns orderrules.flow
  (:require [clojure.core.async :refer :all]))

(declare ordre)
(declare abonnementer)

(defn step [stepfn c kundeid ordrer abonnementer]
  (go
   (let [val (apply stepfn [kundeid ordrer abonnementer])]
     (println "V1" val)
     (>! c val)))
  (go
   (let [val (<! c)]
     (println "V2" val)
     val)))

(defn extract-order [ordrer aftaletype pg-type]
  (let [o (first (filter #(and (= aftaletype (:aftaletype %)) (= pg-type (:pg-type %))) ordrer))
        hw (not (empty? (filter #(and (= aftaletype (:aftaletype %)) (:hw %)) ordrer)))
        tekniker (not (empty? (filter #(and (= aftaletype (:aftaletype %)) (:tekniker %)) ordrer)))]
    (assoc o :hw hw :tekniker tekniker)))

(defn extract-abonnement [abonnementer aftaletype pg-type]
  (filter #(and (= aftaletype (:aftaletype %)) (= pg-type (:pg-type %))) abonnementer))

(defn create-ga-afventer [item]
  (let [orig-size (count @abonnementer)]
    (swap! abonnementer (fn [a] (conj a item)))
    (if (< orig-size (count @abonnementer))
      :ok
      :done)))

(defn opret-clear-abon-ga-afventer-step [kundeid ordrer abonnementer]
  (prn "clear ga")
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer :clear :ga)]
    (if (and o (empty? (extract-abonnement abonnementer :clear :ga)))
      (create-ga-afventer o)
      :done)))

(defn opret-bb-abon-ga-afventer-step [kundeid ordrer abonnementer]
  (prn "bb")
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer :bb :ga)]
    (if (and o (empty? (extract-abonnement abonnementer :bb :ga)))
      (create-ga-afventer o)
      :done)))

(defn opret-tlf-abon-ga-afventer-step [kundeid ordrer abonnementer]
  (prn "tlf")
  (Thread/sleep (rand-int 1000))
  (let [o (extract-order ordrer :tlf :ga)]
    (if (and o (empty? (extract-abonnement abonnementer :tlf :ga)))
      (create-ga-afventer o)
      :done)))

(defn skift-tlf-abon-step [kundeid ordrer abonnementer]
  (prn "tlf")
  (Thread/sleep (rand-int 1000))
  :done)

(defn provisioner-bb-abon-step [kundeid ordrer abonnementer]
  (prn "bbp")
  (Thread/sleep (rand-int 1000))
  :done)

(defn provisioner-tlf-abon-step [kundeid ordrer abonnementer]
  (prn "tlfp")
  (Thread/sleep (rand-int 1000))
  :done)

(def all-steps [opret-bb-abon-ga-afventer-step opret-clear-abon-ga-afventer-step opret-tlf-abon-ga-afventer-step provisioner-bb-abon-step provisioner-tlf-abon-step skift-tlf-abon-step])

(defn find-kunde-ordrer [kundeid]
  [ordre])

(defn find-kunde-abonnementer [kundeid]
  abonnementer)

(defn ordre [kundeid]
  (prn "BUH")
  (let [c (chan)
        ordrer (find-kunde-ordrer kundeid)
        _ (prn ordrer)
        abons @(find-kunde-abonnementer kundeid)
        _ (prn abons)]
    (prn ordrer abons)
    (loop [res [] i 0]
      (if (and (not= i 0) (empty? (filter #(= :ok %) res)))
        (do (prn "BLAAAA" res i)
            (close! c))
        (do (prn "TEST" res i)
            (recur (mapv <!! (map #(step % c kundeid ordrer abons) all-steps)) (inc i)))))))

(def ordre (clojure.walk/keywordize-keys {"id" "2950aa52-f3ac-4f64-9d3b-922ac35adfdb"
             "bestilling" {"kundeid" "606125929"
                           "handlinger" [{"index" 0
                                          "handling" "SKIFT-PRODUKT"
                                          "handlingsdato" "06-02-2013"
                                          "varenr" "1401009"
                                          "aftaletype" "tlf"
                                          "pg-type" "ga"
                                          "hw" false
                                          "tekniker" false
                                          "abonnementsid" "20000135"
                                          "installation" {"amsid" "452232"
                                                          "instnr" "155671"}}
                                         {"varenr" "1417015"
                                          "handling" "OPRET"
                                          "aftaletype" "tlf"
                                          "pg-type" "ky"
                                          "hw" true
                                          "tekniker" false
                                          "installation" {"amsid" "452232"
                                                          "instnr" "155671"}
                                          "index" 1
                                          "aftalenr" "1101927"}
                                         ]
                           "info" {"betaler-brev" false
                                   "klient-funktion" "k2uiBestil"
                                   "juridisk-brev" true
                                   "salgskanal" "K"
                                   "klient-system" "SPOC"
                                   "overstyret-salgskanal" "K"
                                   "klient-bruger" "a65973"
                                   "ordre-kvittering-email" "youseekvitteringer@gmail.com"}}
             "status" "Afsluttet"
             "ordredato" "06-02-2013"
             "version" "3.0"
             "k2orderid" "S01TSPOC----------------79959793"}))

(def abonnementer (atom [{:varenr "1101201"
                          :aftaletype "clear"
                          :pg-type "ga"
                          :status "aktiv"
                          :abonnementsid "20000101"}
                         {:varenr "1301201"
                          :aftaletype "bb"
                          :pg-type "ga"
                          :status "aktiv"
                          :abonnementsid "20000102"}]))

(defn test []
  (let [c (chan)]
    (go
     (>! c :ok))
    (<!! (go
      (let [val (<! c)]
        val)))))
