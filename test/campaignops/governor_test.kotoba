(ns campaignops.governor-test
  (:require [campaignops.advisor :as advisor]
            [campaignops.governor :as sut]
            [campaignops.phase :as phase]
            [campaignops.store :as store]
            [clojure.test :refer [deftest is testing]]))

(def ctx {:actor-id "campaign-test" :phase 3 :now "2026-08-01T00:00:00Z"})

(defn- verdict [st request]
  (sut/check request ctx (advisor/infer st request) st))

(defn- rules [v] (set (map :rule (:violations v))))

;; ───────────────────────── the happy path ─────────────────────────

(deftest a-clean-review-commits-and-a-launch-still-escalates
  (let [st (store/seed-db)]
    (let [v (verdict st {:op :review-campaign :campaign-id "cf-ready"})]
      (is (true? (:ok? v)))
      (is (= [] (:violations v)))
      (is (= :commit (phase/verdict->disposition v))))
    (let [v (verdict st {:op :launch-campaign :campaign-id "cf-ready"})]
      (is (false? (:hard? v)) "nothing is wrong with it")
      (is (true? (:high-stakes? v)))
      (is (= :escalate (phase/verdict->disposition v))
          "letting a stranger take money from the public has an author"))))

(deftest default-mock-advisor-proposals-never-self-trip-scope-exclusion
  (testing "the terms are phrased as completed acts so the happy path survives them"
    (let [st (store/seed-db)]
      (doseq [op [:review-campaign :launch-campaign :suspend-campaign
                  :flag-campaign-concern]]
        (is (not (contains? (rules (verdict st {:op op :campaign-id "cf-ready"
                                                :patch {:reason :test}}))
                            :scope-excluded))
            (str op " must not block itself"))))))

;; ───────────────────────── hard checks ─────────────────────────

(deftest a-security-shaped-campaign-is-permanently-out-of-scope
  (let [st (store/seed-db)]
    (doseq [op [:review-campaign :launch-campaign]]
      (is (contains? (rules (verdict st {:op op :campaign-id "cf-secured"}))
                     :prohibited-category)
          "a 'reward' that is a share of future revenue is a security"))))

(deftest thin-verification-against-a-large-ask-is-refused-before-a-human-is-asked
  (let [st (store/seed-db)
        v  (verdict st {:op :launch-campaign :campaign-id "cf-thin"})]
    (is (true? (:hard? v)))
    (is (contains? (rules v) :blocking-finding))
    (is (some #{"insufficient-verification"} (map :detail (:violations v))))
    (is (some #{"aml-not-run"} (map :detail (:violations v)))
        "'not run' and 'clear' are different statements")))

(deftest the-review-is-re-derived-from-the-store-not-read-from-the-proposal
  (testing "an advisor claiming a clean review over a thin creator is ignored"
    (let [st (store/seed-db)
          lying {:op :launch-campaign :campaign-id "cf-thin" :effect :propose
                 :confidence 0.99
                 :summary "cf-thin は審査を通過している"
                 :value {:campaign-id "cf-thin"
                         :review {:review/campaign "cf-thin" :review/findings []
                                  :review/blocking-count 0 :review/clean? true}}}]
      (is (contains? (rules (sut/check {} ctx lying st)) :blocking-finding)))))

(deftest a-waiver-for-a-rule-that-never-fired-is-a-waiver-being-manufactured
  (let [st (store/seed-db)
        p  {:op :launch-campaign :campaign-id "cf-ready" :effect :propose
            :confidence 0.9 :summary "公開の承認を依頼"
            :value {:campaign-id "cf-ready" :waived [:prohibited-category]}}]
    (is (contains? (rules (sut/check {} ctx p st)) :waiver-for-unraised-rule))))

(deftest a-waiver-for-a-rule-that-did-fire-is-honoured
  (let [st (store/mem-store
            (assoc (store/demo-data)
                   :assertions {"cf-ready" {:prototype-exists? true}}
                   :disclosures {"cf-ready" #{:risks :production-stage
                                              :fulfilment-experience}}))
        p  {:op :launch-campaign :campaign-id "cf-ready" :effect :propose
            :confidence 0.9 :summary "公開の承認を依頼"
            :value {:campaign-id "cf-ready" :waived [:missing-disclosure]}}]
    (is (contains? (rules (sut/check {} ctx (assoc-in p [:value :waived] []) st))
                   :blocking-finding))
    (is (= #{} (rules (sut/check {} ctx p st)))
        "a reviewer who cannot override a rule will route around the system")))

(deftest an-already-live-campaign-cannot-be-launched-again
  (let [st (store/seed-db)]
    (is (contains? (rules (verdict st {:op :launch-campaign :campaign-id "cf-live"}))
                   :illegal-transition))))

(deftest a-structurally-invalid-campaign-never-reaches-a-human
  (let [st (store/mem-store
            (assoc-in (store/demo-data) [:campaigns "cf-ready" :campaign/risks] ""))
        v  (verdict st {:op :review-campaign :campaign-id "cf-ready"})]
    (is (contains? (rules v) :missing-risks-disclosure))))

(deftest an-effect-other-than-propose-is-a-claim-to-actuate
  (let [st (store/seed-db)
        p  (assoc (advisor/infer st {:op :review-campaign :campaign-id "cf-ready"})
                  :effect :execute)]
    (is (contains? (rules (sut/check {} ctx p st)) :effect-not-propose))))

(deftest claiming-to-have-already-acted-is-permanently-blocked
  (let [st (store/seed-db)
        p  (advisor/infer st {:op :launch-campaign :campaign-id "cf-ready"
                              :out-of-scope? true})]
    (is (contains? (rules (sut/check {} ctx p st)) :scope-excluded))))

(deftest ops-outside-the-allowlist-are-refused
  (let [st (store/seed-db)]
    (is (contains? (rules (sut/check {} ctx {:op :charge-backers :effect :propose} st))
                   :op-not-allowed))))

;; ───────────────────────── soft gates ─────────────────────────

(deftest low-confidence-escalates-without-being-a-violation
  (let [st (store/seed-db)
        p  (assoc (advisor/infer st {:op :review-campaign :campaign-id "cf-ready"})
                  :confidence 0.2)
        v  (sut/check {} ctx p st)]
    (is (false? (:hard? v)))
    (is (true? (:escalate? v)))
    (is (= :escalate (phase/verdict->disposition v)))))

;; ───────────────────────── the two-layer invariant ─────────────────────────

(deftest the-consequential-ops-are-out-of-every-phase-auto-set
  (testing "governor and phase agree independently — two layers, not one"
    (doseq [[p {:keys [auto]}] phase/phases
            op sut/always-escalate-ops]
      (is (not (contains? auto op))
          (str op " must never be auto-committable, including phase " p)))))

(deftest a-governor-hold-survives-every-phase
  (doseq [p (keys phase/phases)]
    (is (= :hold (:disposition (phase/gate p {:op :review-campaign} :hold)))
        "compliance wins over rollout")))

(deftest early-phases-disable-writes-rather-than-quietly-allowing-them
  (is (= {:disposition :hold :reason :phase-disabled}
         (phase/gate 0 {:op :draft-campaign} :commit)))
  (is (= {:disposition :escalate :reason :phase-approval}
         (phase/gate 1 {:op :draft-campaign} :commit)))
  (is (= {:disposition :commit :reason nil}
         (phase/gate 3 {:op :draft-campaign} :commit))))
