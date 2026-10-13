# `agent-implements`

The agent writes the code and commits it, then stops. The agent does not
push; the user collapses the commits.

Session ends with: what was written, what was verified (with real numbers),
and anything the reviewer should look at first--especially places where the
implementation deviated from the plan, and why.

## Review

One unit: the commits written this session, named by subject on their branch.

If the session ends with commits the user has not reviewed, add an item to
`review.md` naming them by subject on their branch, in the update whose log
entry briefs the reviewer. If the user reviews them in
session, nothing is added and the log entry records the acceptance.
