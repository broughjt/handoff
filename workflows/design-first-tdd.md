# `design-first-tdd`

Settle a design with the user, pin it in failing tests, and have a fresh
agent implement it from a plan, with the user reviewing each stage before the
next begins.

The full loop, for work where the output format or interface is the hard part:

1. **Design conversation**: work out the design with the user. Present decisions
   through the harness's structured question tool when available, with the
   recommendation first and labelled `(Recommended)`. Interview until genuinely
   aligned, not until the user stops objecting.
2. **Plan document**: `arc:plan-<task>.md`, written so a
   *different* agent can implement from it without the design conversation's
   context.
3. **Tests first**: rendering/unit tests that pin the output format, and
   scenario/integration tests for the paths the design conversation
   identified. Written and left **red** before implementation exists.
4. **Implement**: a fresh agent reads the handoff doc and the plan doc, then
   makes the tests green. While the task is in flight the arc's `Read first`
   list names the plan. The plan doc is removed with `git rm` when the task
   lands, after anything durable in it has been promoted.

## Review

Three units, one per stage:

| After | Item in `review.md` names |
|---|---|
| the plan document | the plan, `arc:plan-<task>.md` |
| the tests, committed red | the tests, by commit subject on their branch |
| the implementation | the implementation, by commit subject on its branch |

If a session ends with a stage the user has not reviewed, add the
corresponding item above to `review.md`. If the user accepts it in session,
nothing is added and the log entry records the acceptance. Each stage still
waits on the user before the next begins: the next stage does not begin while
the previous one is on the list, since the design is settled before tests pin
it, and the tests are settled before implementation. `Next` may name other
work meanwhile.
