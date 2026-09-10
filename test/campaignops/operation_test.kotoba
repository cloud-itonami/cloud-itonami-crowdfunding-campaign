(ns campaignops.operation-test
  (:require [campaignops.operation :as operation]
            [campaignops.store :as store]
            [clojure.test :refer [deftest is testing]]
            [langgraph.graph :as g]))

(def ctx {:actor-id "campaign-test" :phase 3 :now "2026-08-01T00:00:00Z"})

(defn- run-req! [actor tid request]
  (g/run* actor {:request request :context ctx} {:thread-id tid}))

(deftest a-review-auto-commits-and-changes-no-campaign-state
  (let [s (store/seed-db)
        a (operation/build s)
        r (run-req! a "t1" {:op :review-campaign :campaign-id "cf-ready"})]
    (is (= :commit (:disposition (:state r))))
    (is (= :in-review (:campaign/state (store/campaign s "cf-ready")))
        "findings are not a verdict, so recording them moves nothing")
    (is (some #(= :committed (:t %)) (store/ledger s)))))

(deftest a-launch-interrupts-and-only-a-named-human-completes-it
  (let [s (store/seed-db)
        a (operation/build s)
        held (run-req! a "t2" {:op :launch-campaign :campaign-id "cf-ready"})]
    (is (= :in-review (:campaign/state (store/campaign s "cf-ready")))
        "nothing committed before approval")
    (is (some #(= :approval-requested (:t %)) (:audit (:state held))))
    (testing "and the approver's name lands on the record"
      (let [ok (g/run* a {:approval {:status :approved :by "trust-01"}}
                       {:thread-id "t2" :resume? true})]
        (is (= :commit (:disposition (:state ok))))
        (is (= :live (:campaign/state (store/campaign s "cf-ready"))))
        (is (= "trust-01" (:decision/decided-by (store/decision s "cf-ready"))))
        (is (true? (:decision/human? (store/decision s "cf-ready"))))))))

(deftest a-rejected-approval-holds-and-leaves-the-campaign-alone
  (let [s (store/seed-db)
        a (operation/build s)]
    (run-req! a "t3" {:op :launch-campaign :campaign-id "cf-ready"})
    (let [no (g/run* a {:approval {:status :rejected :by "trust-01"}}
                     {:thread-id "t3" :resume? true})]
      (is (= :hold (:disposition (:state no))))
      (is (= :in-review (:campaign/state (store/campaign s "cf-ready"))))
      (is (some #(= :approval-rejected (:t %)) (store/ledger s))))))

(deftest a-hard-violation-never-reaches-the-interrupt
  (let [s (store/seed-db)
        a (operation/build s)
        r (run-req! a "t4" {:op :launch-campaign :campaign-id "cf-secured"})]
    (is (= :hold (:disposition (:state r))))
    (is (not-any? #(= :approval-requested (:t %)) (:audit (:state r)))
        "a human is not asked to approve a security offering")
    (is (contains? (set (:basis (last (store/ledger s)))) :prohibited-category))))

(deftest the-ledger-records-holds-as-well-as-commits
  (let [s (store/seed-db)
        a (operation/build s)]
    (run-req! a "t5" {:op :launch-campaign :campaign-id "cf-thin"})
    (let [f (last (store/ledger s))]
      (is (= :governor-hold (:t f)))
      (is (= :hold (:disposition f)))
      (is (seq (:violations f)) "a hold with no stated basis cannot be appealed"))))

(deftest phase-zero-writes-nothing
  (let [s (store/seed-db)
        a (operation/build s)
        r (g/run* a {:request {:op :draft-campaign :campaign-id "cf-new"
                               :patch {:creator "creator.alpha" :title "x"
                                       :currency "JPY" :goal-minor 1000000
                                       :category :technology :duration-days 30
                                       :story "s" :risks "r"}}
                     :context (assoc ctx :phase 0)}
                   {:thread-id "t6"})]
    (is (= :hold (:disposition (:state r))))
    (is (nil? (store/campaign s "cf-new")))))
