# cloud-itonami-crowdfunding-campaign

**CampaignAdvisor ⊣ CampaignGovernor** — the actor that decides nothing
about whether a campaign may launch, and makes sure a named human does.

A `cloud-itonami` blueprint actor in the workspace's standard shape: an
LLM advisor sealed into one node of a [langgraph-clj](https://github.com/kotoba-lang/langgraph)
StateGraph, an independent governor able to reject anything it proposes,
a 0→3 rollout phase gate, and an append-only audit ledger. Domain rules
come from [`kotoba-lang/crowdfunding`](https://github.com/kotoba-lang/crowdfunding);
design record: ADR-2607268500.

## Why this actor exists

Launching a campaign grants a stranger the ability to take money from the
public against a promise. Everything downstream — pledges, collection,
payout — is gated on a campaign being live, so this is the moment where
review has to actually happen.

`crowdfunding.trust/review` produces **findings**, never a verdict. There
is no `approve` function anywhere in the stack. This actor runs the rule
set, hands the result to a person, and records who decided.

## The invariant

> The governor rejects; the actor never writes what it refuses.

Six HARD checks, un-overridable by any human approval:

| Check | Why |
|---|---|
| Structural invalidity | no risks disclosure, 90-day window, zero goal |
| Blocking review findings | re-derived from the **store's** evidence, not the proposal's claim |
| Prohibited category | equity / revenue-share / loan / lottery — a security is not a reward, and this is a permanent scope exclusion |
| Illegal state transition | launching an already-live campaign |
| `:effect` ≠ `:propose` | a claim to actuate outside governance |
| Scope exclusion | claiming to have already launched, suspended, charged or refunded |

Two SOFT gates escalate to a human: low advisor confidence, and
`:launch-campaign` / `:suspend-campaign` / `:flag-campaign-concern` —
**always**, in every phase including 3. `phase.cljc` keeps them out of
every `:auto` set independently of the governor: two layers, not one.

## A waiver must name its rule

A reviewer who cannot override a rule will route around the system, so
blocking findings *can* be waived — but only per-rule, and only for a
rule that actually fired. A waiver for a rule that was never raised is
refused (`:waiver-for-unraised-rule`): that is a record of an override
that never happened.

## Run it

```bash
clojure -M:dev:run     # offline demo: review, refusals, a human-gated launch
clojure -M:dev:test    # 22 tests
clojure -M:lint
```

The demo shows a thin-evidence campaign and a revenue-share campaign both
refused *before* a human is asked, a clean campaign waiting at the
interrupt, and the approver's name landing on the record.

## Licence

AGPL-3.0-or-later.
