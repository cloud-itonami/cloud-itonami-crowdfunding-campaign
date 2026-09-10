(ns campaignops.store
  "SSoT for the campaign actor — which campaigns exist, what evidence the
  platform actually holds about their creators, and what a human decided.

  Directories, all keyed by STRING ids (never keywords):

    campaigns    campaign id -> campaign record
    rewards      campaign id -> reward tiers
    disclosures  campaign id -> set of disclosures the creator supplied
    evidence     creator id  -> verification outcomes. This is the ONLY
                 thing the governor trusts about a creator; a proposal
                 claiming a creator is verified is ignored. eKYC and AML
                 run elsewhere and write their outcomes here.
    context      campaign id -> what the creator asserted about the
                 project (prototype exists, imagery kinds). Assertions,
                 labelled as such, never treated as findings.
    decisions    campaign id -> the human launch decision, if any

  This store holds no money. Its most consequential record is a launch
  decision, because launching a campaign is the act of letting a stranger
  take money from the public.

  The ledger stays append-only."
  (:require [crowdfunding.campaign :as campaign]
            [crowdfunding.reward :as reward]
            [crowdfunding.trust :as trust]))

(defprotocol Store
  (campaign [s id] "Campaign record, or nil.")
  (all-campaigns [s])
  (rewards [s id] "Reward tiers for a campaign.")
  (disclosures [s id] "Disclosures the creator supplied.")
  (evidence [s creator] "Verification outcomes held for a creator — ground truth.")
  (assertions [s id] "What the creator asserted about the project.")
  (decision [s id] "The human launch decision, or nil.")
  (ledger [s])
  (campaign-log [s])
  (commit-record! [s record])
  (append-ledger! [s fact]))

;; ----------------------------- demo data -----------------------------

(def ^:private risks
  (str "Tooling for the split chassis is not finalised and the injection "
       "moulder has quoted a six week lead time that could slip. The FIDO2 "
       "controller is single-source; a shortage would delay every tier."))

(defn- a-campaign [id creator goal state]
  (assoc (campaign/campaign
          {:id id :creator creator :title (str id " campaign")
           :currency "JPY" :goal-minor goal :category :technology
           :duration-days 45 :launched-at "2026-08-01T00:00:00Z"
           :deadline "2026-09-15T00:00:00Z"
           :story "A split mechanical keyboard with an on-board authenticator."
           :risks risks :ships-to #{:jp :rest-of-world}})
         :campaign/state state))

(defn demo-data
  "Self-contained fixtures covering the happy path and each hard check.

    cf-ready    creator fully verified, complete disclosures  -> launchable
    cf-thin     creator verified only by email  -> insufficient verification
    cf-secured  category :equity  -> a security, permanently out of scope
    cf-live     already live, so a second launch is a table violation"
  []
  {:campaigns
   {"cf-ready"   (a-campaign "cf-ready" "creator.alpha" 3000000 :in-review)
    "cf-thin"    (a-campaign "cf-thin" "creator.beta" 50000000 :in-review)
    "cf-secured" (assoc (a-campaign "cf-secured" "creator.gamma" 1000000 :in-review)
                        :campaign/category :equity)
    "cf-live"    (a-campaign "cf-live" "creator.alpha" 2000000 :live)}
   :rewards
   (let [tiers [(reward/reward {:id "early" :title "Early Bird" :minimum-minor 19800
                                :limit 50 :estimated-delivery "2027-03-01"
                                :shipping {:jp 800 :rest-of-world 3000}
                                :ships-to #{:jp :rest-of-world}})
                (reward/reward {:id "standard" :title "Standard" :minimum-minor 24800
                                :estimated-delivery "2027-04-01"
                                :shipping {:jp 800 :rest-of-world 3000}
                                :ships-to #{:jp :rest-of-world}})]]
     {"cf-ready" tiers "cf-thin" tiers "cf-secured" tiers "cf-live" tiers})
   :disclosures
   {"cf-ready"   trust/required-disclosures
    "cf-thin"    trust/required-disclosures
    "cf-secured" trust/required-disclosures
    "cf-live"    trust/required-disclosures}
   :evidence
   {"creator.alpha" {:email true :payment-method true :identity-document true
                     :address-proof true :sanctions-screen true :aml/status :clear}
    "creator.beta"  {:email true}
    "creator.gamma" {:email true :payment-method true :identity-document true
                     :address-proof true :sanctions-screen true :aml/status :clear}}
   :assertions
   {"cf-ready"   {:prototype-exists? true}
    "cf-thin"    {:prototype-exists? false :imagery [:photoreal-render]}
    "cf-secured" {:prototype-exists? true}
    "cf-live"    {:prototype-exists? true}}
   :decisions {}})

;; ----------------------------- MemStore -----------------------------

(defrecord MemStore [a]
  Store
  (campaign [_ id] (get-in @a [:campaigns id]))
  (all-campaigns [_] (sort-by :campaign/id (vals (:campaigns @a))))
  (rewards [_ id] (get-in @a [:rewards id] []))
  (disclosures [_ id] (get-in @a [:disclosures id] #{}))
  (evidence [_ creator] (get-in @a [:evidence creator] {}))
  (assertions [_ id] (get-in @a [:assertions id] {}))
  (decision [_ id] (get-in @a [:decisions id]))
  (ledger [_] (:ledger @a))
  (campaign-log [_] (:campaign-log @a))
  (commit-record! [_ record]
    (swap! a update :campaign-log conj record)
    (let [{:keys [op value payload]} record
          id (:campaign-id value)]
      (case op
        :draft-campaign
        (when-let [c (:campaign value)] (swap! a assoc-in [:campaigns (:campaign/id c)] c))

        ;; A review is a set of FINDINGS, never a verdict. Recording it
        ;; changes no campaign state — that is what keeps `:in-review`
        ;; something a human still has to act on.
        :review-campaign
        (swap! a assoc-in [:reviews id] (:review value))

        :launch-campaign
        (swap! a
               (fn [m]
                 (-> m
                     (update-in [:campaigns id] #(when % (campaign/advance % :live)))
                     (assoc-in [:decisions id]
                               {:decision/outcome :approved
                                ;; The approver comes from :payload, where
                                ;; the operation graph stamps it. Reading it
                                ;; from :value would record every launch as
                                ;; unattributed.
                                :decision/decided-by (:approved-by payload)
                                :decision/human? true}))))

        :suspend-campaign
        (swap! a update-in [:campaigns id]
               #(when % (or (campaign/suspend % {:by (:approved-by payload)
                                                 :at (:at value)
                                                 :reason (:reason value)})
                            %)))
        nil))
    record)
  (append-ledger! [_ fact] (swap! a update :ledger conj fact) fact))

(defn seed-db []
  (->MemStore (atom (assoc (demo-data) :ledger [] :campaign-log [] :reviews {}))))

(defn mem-store [m]
  (->MemStore (atom (merge {:campaigns {} :rewards {} :disclosures {} :evidence {}
                            :assertions {} :decisions {} :reviews {}
                            :ledger [] :campaign-log []}
                           m))))

;; ----------------------------- derived views -----------------------------

(defn review-for
  "Run `crowdfunding.trust/review` over a campaign using THIS store's
  evidence and disclosures.

  That the evidence comes from the store rather than from the request is
  the whole point: a creator (or an advisor) claiming to be verified is a
  claim, and this actor's governor only ever reads the record."
  [s id]
  (when-let [c (campaign s id)]
    (trust/review {:campaign    c
                   :rewards     (rewards s id)
                   :disclosures (disclosures s id)
                   :evidence    (evidence s (:campaign/creator c))
                   :context     (assertions s id)})))
