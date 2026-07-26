(ns campaignops.governor
  "CampaignGovernor — the independent compliance layer standing between a
  proposed campaign and the public's money.

  The advisor has no notion of whether the campaign it wants to launch is
  structurally valid, whether its creator is verified to the level the ask
  requires, whether its category is a security dressed as a reward, or
  whether its own `:effect` secretly claims to have already launched
  something. So this MUST be a separate system able to *reject* a proposal
  and fall back to HOLD.

  ## Launching is the consequential act

  Everything downstream — pledges, collection, payout — is gated on a
  campaign being live. Launch is where a stranger is granted the ability
  to take money from the public against a promise, and it is the one act
  no phase of this actor ever auto-commits.

  Six HARD checks, ALL permanent, un-overridable by any human approval:

    1. Structural invalidity   -- `crowdfunding.campaign/campaign-errors`
                                  must be empty. A campaign with no risks
                                  disclosure or a 90-day window is refused
                                  before a human is asked about it.
    2. Blocking review findings -- re-derived HERE from the store's own
                                  evidence, never read from the proposal.
                                  A finding waived in the request is only
                                  honoured if it was actually raised; a
                                  waiver for a rule that did not fire is a
                                  waiver being manufactured.
    3. Prohibited category     -- equity, revenue-share, loan, lottery. A
                                  reward-crowdfunding actor issuing
                                  securities is a permanent scope
                                  exclusion, not a missing feature.
    4. Illegal state transition -- launching an already-live campaign, or
                                  suspending one the table forbids.
    5. Effect not :propose     -- any other value is a claim to directly
                                  actuate outside governance.
    6. Scope exclusion         -- any claim to have launched, suspended,
                                  charged or refunded, plus any op outside
                                  the closed allowlist.

  Two ESCALATE (SOFT) gates:
    - LLM confidence below the floor.
    - `:launch-campaign`, `:suspend-campaign` and `:flag-campaign-concern`
      ALWAYS escalate. `campaignops.phase` keeps all three out of every
      phase's `:auto` set independently — two layers, not one."
  (:require [campaignops.store :as store]
            [clojure.string :as str]
            [crowdfunding.campaign :as cf]
            [crowdfunding.trust :as trust]))

(def confidence-floor 0.6)

(def allowed-ops
  "The closed proposal-op allowlist. CRITICAL: no op that decides a
  review outcome on its own is EVER a member. `:launch-campaign`
  REQUESTS an approval; it does not grant one."
  #{:draft-campaign :review-campaign :launch-campaign
    :suspend-campaign :flag-campaign-concern})

(def always-escalate-ops
  #{:launch-campaign :suspend-campaign :flag-campaign-concern})

(def scope-excluded-terms
  "Case-insensitive substrings marking a proposal as claiming to have
  already acted.

  CRITICAL: every term is phrased as the COMPLETED act ('launched the
  campaign'), never a bare noun like 'launch' or 'campaign' — a bare noun
  would match inside this actor's own legitimate proposals (whose whole
  job is to talk about launching campaigns) and self-block the happy
  path. See
  `campaignops.governor-test/default-mock-advisor-proposals-never-self-trip-scope-exclusion`."
  ["launched the campaign" "have launched" "published the campaign"
   "approved the launch" "have approved" "granted the approval"
   "suspended the campaign" "have suspended" "lifted the suspension"
   "charged the backers" "collected the pledges" "refunded the backers"
   "issued the securities" "sold the shares"
   "公開した" "公開を実行した" "承認した" "停止した" "停止を実行した"
   "解除した" "支援者に請求した" "返金した"])

;; ----------------------------- checks -----------------------------

(defn- target
  "The campaign a proposal is about: the one it carries, or the stored
  record for the id it names."
  [proposal st]
  (or (get-in proposal [:value :campaign])
      (some->> (get-in proposal [:value :campaign-id]) (store/campaign st))))

(defn- structural-violations
  "Applies to any op that carries or names a campaign. A campaign that is
  not structurally valid must not become a draft, a review or a launch."
  [proposal st]
  (when (contains? #{:draft-campaign :review-campaign :launch-campaign} (:op proposal))
    (if-let [c (target proposal st)]
      (when-let [errs (seq (cf/campaign-errors c))]
        (mapv (fn [e] {:rule (:campaign.error/code e)
                       :detail (or (:campaign.error/detail e)
                                   (name (:campaign.error/code e)))})
              errs))
      [{:rule :campaign-missing :detail "対象のキャンペーンが特定できない"}])))

(defn- category-violations
  "A prohibited category is a HARD block at every op, including a mere
  draft. `crowdfunding.trust/prohibited-categories` puts equity and
  revenue-share there because a 'reward' that is a share of future revenue
  is a security, and belongs under a different regime entirely — not
  behind a stricter review in this one."
  [proposal st]
  (when-let [c (target proposal st)]
    (when (contains? trust/prohibited-categories (:campaign/category c))
      [{:rule :prohibited-category
        :detail (str (pr-str (:campaign/category c))
                     " は報酬型クラウドファンディングの対象外(証券・貸付・くじ等)")}])))

(defn- review-violations
  "For `:launch-campaign`: re-run the review from the STORE and refuse any
  blocking finding the request did not explicitly waive.

  Two things are deliberate. First, the review is re-derived rather than
  read from the proposal — the 'ground truth, not self-report' rule.
  Second, a waiver is only honoured for a rule that actually fired: an
  advisor listing waivers for rules that were never raised would be
  building a record of overrides that never happened."
  [proposal st]
  (when (= :launch-campaign (:op proposal))
    (let [id     (get-in proposal [:value :campaign-id])
          r      (store/review-for st id)
          waived (set (get-in proposal [:value :waived]))
          raised (set (map :finding/rule (trust/launch-blocked-by r)))]
      (cond
        (nil? r)
        [{:rule :campaign-missing :detail (str (or id "(id missing)") " は存在しない")}]

        (seq (remove waived raised))
        (mapv (fn [rule] {:rule :blocking-finding :detail (name rule)})
              (sort (remove waived raised)))

        (seq (remove raised waived))
        (mapv (fn [rule] {:rule :waiver-for-unraised-rule :detail (str rule)})
              (sort (remove raised waived)))))))

(defn- transition-violations
  "Launching an already-live campaign, or suspending one the table
  forbids. Delegated to `crowdfunding.campaign/transitions` rather than
  restated here, so there is one table and not two that can drift."
  [proposal st]
  (let [op (:op proposal)
        c  (target proposal st)
        to (case op :launch-campaign :live :suspend-campaign :suspended nil)]
    (when (and to c)
      (when-not (contains? (get cf/transitions (:campaign/state c) #{}) to)
        [{:rule :illegal-transition
          :detail (str (pr-str (:campaign/state c)) " -> " (pr-str to) " は許可されていない")}]))))

(defn- effect-not-propose-violations [proposal]
  (when (not= :propose (:effect proposal))
    [{:rule :effect-not-propose
      :detail (str ":effect は :propose のみ許可されるが " (pr-str (:effect proposal)) " が提案された")}]))

(defn- text-blob [proposal]
  (str/lower-case (pr-str (select-keys proposal [:op :summary :rationale :cites]))))

(defn- scope-exclusion-violations [proposal]
  (let [op (:op proposal)
        blob (text-blob proposal)]
    (cond
      (not (contains? allowed-ops op))
      [{:rule :op-not-allowed
        :detail (str (pr-str op) " は許可された操作(closed allowlist)に含まれない")}]

      (some #(str/includes? blob %) scope-excluded-terms)
      [{:rule :scope-excluded
        :detail "公開・停止・請求・返金を実行済みと主張する提案は永久に禁止"}])))

(defn check
  "Censors a CampaignAdvisor proposal.

  Returns {:ok? bool :violations [..] :confidence c :escalate? bool
           :high-stakes? bool :hard? bool}."
  [_request _context proposal store]
  (let [hard (into []
                   (concat (structural-violations proposal store)
                           (category-violations proposal store)
                           (review-violations proposal store)
                           (transition-violations proposal store)
                           (effect-not-propose-violations proposal)
                           (scope-exclusion-violations proposal)))
        conf (:confidence proposal 0.0)
        low? (< conf confidence-floor)
        stakes? (boolean (always-escalate-ops (:op proposal)))
        hard? (boolean (seq hard))]
    {:ok?          (and (not hard?) (not low?) (not stakes?))
     :violations   hard
     :confidence   conf
     :hard?        hard?
     :escalate?    (and (not hard?) (or low? stakes?))
     :high-stakes? stakes?}))

(defn hold-fact [request context verdict]
  {:t           :governor-hold
   :op          (:op request)
   :actor       (:actor-id context)
   :campaign-id (:campaign-id request)
   :disposition :hold
   :basis       (mapv :rule (:violations verdict))
   :violations  (:violations verdict)
   :confidence  (:confidence verdict)})
