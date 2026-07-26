(ns campaignops.phase
  "Phase 0->3 staged rollout for the campaign actor.

    Phase 0  read-only        -- no writes, still governor-gated.
    Phase 1  assisted-draft   -- drafts may be recorded, every write needs
                                 human approval.
    Phase 2  assisted-review  -- adds running the launch review, still
                                 approval-gated.
    Phase 3  supervised auto  -- governor-clean, high-confidence
                                 `:draft-campaign` and `:review-campaign`
                                 may auto-commit.

  `:launch-campaign`, `:suspend-campaign` and `:flag-campaign-concern` are
  deliberately ABSENT from every phase's `:auto` set, INCLUDING phase 3 —
  a permanent structural fact, not a rollout milestone still to come.

  The two that matter:

    - `:launch-campaign` grants a stranger the ability to take money from
      the public against a promise. There is no confidence level at which
      that should happen without a name attached to it.
    - `:suspend-campaign` removes a creator's funding mid-flight. A
      suspension nobody can be asked about cannot be appealed, and an
      actor that could suspend on its own would make suspension worthless
      as a signal.

  Everything auto-committable here is a DRAFT or a set of FINDINGS.
  Nothing auto-committable changes whether a campaign can take money.
  `campaignops.governor`'s own `always-escalate-ops` enforces the same
  invariant independently — two layers, not one, agree on this."
  (:require [campaignops.governor :as governor]))

(def read-ops #{})
(def write-ops governor/allowed-ops)

;; NOTE the invariant: the three consequential ops are members of
;; `write-ops` (governor-gated like any write) but are NEVER members of
;; any phase's `:auto` set below. Do not add them there.
(def phases
  {0 {:label "read-only"       :writes #{}                 :auto #{}}
   1 {:label "assisted-draft"  :writes #{:draft-campaign}  :auto #{}}
   2 {:label "assisted-review" :writes #{:draft-campaign :review-campaign} :auto #{}}
   3 {:label "supervised-auto" :writes write-ops
      :auto #{:draft-campaign :review-campaign}}})

(def default-phase 3)

(defn gate
  "Adjust a governor disposition for the rollout phase. Returns
  {:disposition kw :reason kw|nil}.

  - a governor HOLD always stays HOLD (compliance wins).
  - a write op not yet enabled in this phase -> HOLD (:phase-disabled).
  - a write op enabled but not auto-eligible -> ESCALATE
    (:phase-approval), even if the governor was clean."
  [phase {:keys [op]} governor-disposition]
  (let [{:keys [writes auto]} (get phases phase (get phases default-phase))]
    (cond
      (= :hold governor-disposition)       {:disposition :hold :reason nil}
      (contains? read-ops op)              {:disposition governor-disposition :reason nil}
      (not (contains? writes op))          {:disposition :hold :reason :phase-disabled}
      (and (= :commit governor-disposition)
           (not (contains? auto op)))      {:disposition :escalate :reason :phase-approval}
      :else                                {:disposition governor-disposition :reason nil})))

(defn verdict->disposition
  "Map a CampaignGovernor verdict to a base disposition before the phase
  gate."
  [verdict]
  (cond (:hard? verdict) :hold
        (:escalate? verdict) :escalate
        :else :commit))
