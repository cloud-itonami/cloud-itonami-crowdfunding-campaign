(ns campaignops.sim
  "Offline demo: run a launch review, watch a thin-evidence campaign and a
  security-shaped one get refused, and watch a launch wait for a human.
  `clojure -M:dev:run`."
  (:require [campaignops.operation :as operation]
            [campaignops.store :as store]
            [langgraph.graph :as g]))

(def ^:private ctx {:actor-id "campaign-demo" :phase 3 :now "2026-08-01T00:00:00Z"})

(defn- run-req! [actor tid request]
  (g/run* actor {:request request :context ctx} {:thread-id tid}))

(defn -main [& _]
  (let [s (store/seed-db)
        actor (operation/build s)]

    (println "\n=== 1. 公開前審査（所見の列挙。可否は決めない）===")
    (let [r (run-req! actor "sim-1" {:op :review-campaign :campaign-id "cf-ready"})
          rv (store/review-for s "cf-ready")]
      (println "  status     :" (:status r))
      (println "  阻害所見    :" (:review/blocking-count rv))
      (println "  注意所見    :" (mapv :finding/rule
                                       (filter #(= :advisory (:finding/severity %))
                                               (:review/findings rv))))
      (println "  人間必須    :" (:review/human-required? rv) "／裁定済み:" (:review/adjudicated? rv)))

    (println "\n=== 2. 検証が薄いまま 5,000 万円を募る（HARD hold）===")
    (let [r (run-req! actor "sim-2" {:op :launch-campaign :campaign-id "cf-thin"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 3. 報酬が将来収益の持分（= 証券）: 永久に対象外 ===")
    (let [r (run-req! actor "sim-3" {:op :launch-campaign :campaign-id "cf-secured"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 4. 公開は必ず人間の承認を通る ===")
    (let [held (run-req! actor "sim-4" {:op :launch-campaign :campaign-id "cf-ready"})]
      (println "  status     :" (:status held))
      (println "  状態        :" (:campaign/state (store/campaign s "cf-ready")) "（承認前）")
      (let [ok (g/run* actor {:approval {:status :approved :by "trust-01"}}
                       {:thread-id "sim-4" :resume? true})]
        (println "  --- 人間 trust-01 が承認 ---")
        (println "  status     :" (:status ok))
        (println "  状態        :" (:campaign/state (store/campaign s "cf-ready"))
                 "by" (:decision/decided-by (store/decision s "cf-ready")))))

    (println "\n=== 5. 既に公開中のキャンペーンは二重に公開できない ===")
    (let [r (run-req! actor "sim-5" {:op :launch-campaign :campaign-id "cf-live"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 監査台帳 ===")
    (doseq [f (store/ledger s)]
      (println " " (:t f) (:op f) (or (:basis f) "")))))
