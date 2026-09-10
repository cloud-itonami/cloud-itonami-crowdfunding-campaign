(ns campaignops.advisor
  "CampaignAdvisor — the *contained intelligence node* for the campaign
  actor.

  It drafts exactly five kinds of proposal from a closed allowlist:
  drafting a campaign, running a launch review, proposing a launch,
  proposing a suspension, and flagging a concern.

  CRITICAL: it is a smart-but-untrusted advisor. Every proposal's
  `:effect` is always `:propose`; nothing here launches, suspends or
  approves anything. Every output is censored downstream by
  `campaignops.governor`.

  Note what the advisor does NOT decide. The REVIEW is not its opinion:
  `propose-review` calls `crowdfunding.trust/review`, a pure function over
  the store's own evidence, and the governor independently re-runs it over
  the result. An advisor cannot argue a campaign past a verification floor
  or a prohibited category — it can only propose that the rules be run.

  Like every sibling actor's advisor this is a deterministic mock so the
  actor graph runs offline. In production this calls a real LLM with the
  same proposal shape."
  (:require [campaignops.store :as store]
            [crowdfunding.campaign :as cf]
            [crowdfunding.trust :as trust]))

(defprotocol Advisor
  (-advise [advisor store request] "store + request -> proposal map"))

(defn- propose-draft
  [_st {:keys [campaign-id patch]}]
  (let [c (cf/campaign (assoc patch :id campaign-id))]
    {:op          :draft-campaign
     :campaign-id campaign-id
     :summary     (str campaign-id " の下書き作成を提案: 目標 " (:campaign/goal-minor c)
                       " " (:campaign/currency c)
                       " / " (name (:campaign/funding-model c)))
     :rationale   "公開前の下書き記録の作成のみ。公開も資金の受付も行わない。"
     :cites       [campaign-id]
     :effect      :propose
     :value       {:campaign-id campaign-id :campaign c}
     :confidence  0.9}))

(defn- propose-review
  "Draft a launch review. The findings come from `crowdfunding.trust`
  over the STORE's evidence, not from the model."
  [st {:keys [campaign-id]}]
  (let [r (store/review-for st campaign-id)]
    {:op          :review-campaign
     :campaign-id campaign-id
     :summary     (str campaign-id " の公開前審査: 阻害 " (:review/blocking-count r 0)
                       " 件 / 注意 " (:review/advisory-count r 0) " 件")
     :rationale   "規則に基づく所見の列挙のみ。公開の可否は人間が決める。"
     :cites       [campaign-id]
     :effect      :propose
     :value       {:campaign-id campaign-id :review r}
     :confidence  0.93}))

(defn- propose-launch
  "Propose a launch. ALWAYS escalates — launching is the act of letting a
  stranger take money from the public, and it has an author."
  [st {:keys [campaign-id patch]}]
  (let [r (store/review-for st campaign-id)]
    {:op          :launch-campaign
     :campaign-id campaign-id
     :summary     (str campaign-id " の公開承認を依頼 (阻害所見 "
                       (:review/blocking-count r 0) " 件)")
     :rationale   "審査結果を添えた公開の承認依頼のみ。承認は人間が行い、その名前が記録される。"
     :cites       (vec (keep identity [campaign-id (:review/campaign r)]))
     :effect      :propose
     :value       {:campaign-id campaign-id :review r :waived (vec (:waived patch))}
     :confidence  (or (:confidence patch) 0.85)}))

(defn- propose-suspension
  "Propose a suspension. ALWAYS escalates — a suspension that cannot be
  attributed cannot be appealed."
  [_st {:keys [campaign-id patch]}]
  {:op          :suspend-campaign
   :campaign-id campaign-id
   :summary     (str campaign-id " の一時停止を提案: " (pr-str (:reason patch :unstated)))
   :rationale   "観察された事実に基づく停止の承認依頼のみ。解除は人間の裁定でしか行えない。"
   :cites       [campaign-id]
   :effect      :propose
   :value       {:campaign-id campaign-id :reason (:reason patch) :at (:at patch)}
   :confidence  (or (:confidence patch) 0.8)})

(defn- propose-concern
  [_st {:keys [campaign-id patch]}]
  {:op          :flag-campaign-concern
   :campaign-id campaign-id
   :summary     (str campaign-id " に関する懸念フラグ: " (pr-str (:concern patch "unknown")))
   :rationale   "観察された懸念事実の報告のみ。停止・公開・返金の判断は行わない。"
   :cites       [campaign-id]
   :effect      :propose
   :value       (merge {:campaign-id campaign-id} patch)
   :confidence  (or (:confidence patch) 0.78)})

(defn infer
  [st {:keys [op out-of-scope?] :as request}]
  (let [proposal (case op
                   :draft-campaign        (propose-draft st request)
                   :review-campaign       (propose-review st request)
                   :launch-campaign       (propose-launch st request)
                   :suspend-campaign      (propose-suspension st request)
                   :flag-campaign-concern (propose-concern st request)
                   {})]
    ;; Test hook: inject scope-excluded content to exercise the
    ;; governor's scope-exclusion block end-to-end. Clear before
    ;; production use.
    (if out-of-scope?
      (update proposal :rationale str
              " -- actually launched the campaign and charged the backers")
      proposal)))

(defn trace [_request proposal]
  {:t           :advisor-proposal
   :op          (:op proposal)
   :campaign-id (:campaign-id proposal)
   :summary     (:summary proposal)
   :confidence  (:confidence proposal)})

(defn mock-advisor []
  (reify Advisor
    (-advise [_ store request]
      (infer store request))))

(defn blocking-rules
  "Convenience for callers reading a review out of a proposal."
  [proposal]
  (mapv :finding/rule (trust/launch-blocked-by (get-in proposal [:value :review]))))
