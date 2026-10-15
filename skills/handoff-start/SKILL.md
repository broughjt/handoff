---
name: handoff-start
description: Resume multi-session work from a handoff. Runs `handoff start`, reads its report and the files it points at, interprets what changed since the last handoff, and briefs the user. Companion skills are handoff-create and handoff-update.
---

# Resume from a handoff

Arguments:

- none: find the arc, brief, wait
- `<arc>`: use that arc
- `go`: brief, then begin `next` without waiting
- `<arc> go`: both

## Step 1: Read HANDOFF.md

Run `handoff guide` and read the file at the path it prints, unless you have already read it this session and it is still in view. Reread it after compaction or whenever you are unsure of a rule.

## Step 2: Run `handoff start`

Run `handoff start [arc]` synchronously. It runs the checks, so give it the time they need. Never put it in the background. Special cases:

- Several arcs: if there are several arcs and you were not given one, `handoff start` refuses and lists them. Ask the user which one they want. Do not guess from recency, since the user may be switching arcs deliberately.
- No arcs: say so, offer to run the handoff-create skill, and stop.
- No handoff repository: say so and stop. The user creates it.

## Step 3: Read the report and the files it points to

The report inlines `start.toml`, `arc.md`, the workflow, `tasks.md`, `do-not-reopen.md`, `open-questions.md`, `review.md`, and the latest log entry. Do not read them again unless you mean to modify them.

Read every `read_first` entry the report lists, except those it marks missing, which are discrepancies (step 4). The report lists the arc's other files and the topic documents with their summaries. By default, read these only when their summaries bear directly on `next`. Do not read older log entries unless something in the report is unclear and reading the history would resolve it.

## Step 4: Interpret the report

Report discrepancies. Never reconcile them silently: the user decides whether the documents are inaccurate or something in the environment should be fixed.

Interpret the comparison of the check results against those of the previous handoff:

- A flip the latest log entry does not explain.
- A check that is new, removed, or redefined under the same id.
- Unknown: the check needs better squashing, or something has changed in the environment. Say which you think, and why.
- Timeout: the check is unverified, not contradicted.

Use the latest log entry and the rest of the arc's files to interpret the results. For example, a false check might be expected rather than a discrepancy.

Examine changes in the project repository since the last handoff:

- Commits since the recorded head: work done outside a session, often by the user. Commits the user wrote come before `next`: read them alongside the `read_first` entries, and raise anything in them that changes or contradicts the arc's files. Other commits you cannot account for may be discrepancies worth reporting.
- Rewritten history: map the commit subjects mentioned in the latest log entry and in `review.md` onto the new commits.
- A recorded head that no longer resolves: history was rewritten and pruned, or the last handoff was made on another machine.
- A branch that differs from the one recorded.

Check for discrepancies in the handoff repository, including lint errors, uncommitted changes, and `read_first` entries or a workflow marked missing.

A pending log entry means an update was begun and never committed, so the last handoff is the one before it. Report the entry's host and time and whether it has an `entry.md`, and ask the user whether to finish it (rerun `handoff update --begin`, review the entry, then `handoff update --commit`) or discard it. Do not do either without the user's explicit confirmation.

Also check for review items whose work appears to have completed, rewritten, or removed. Raise each of these as a question to the user, and don't remove them without explicit user confirmation.

## Step 5: Brief

Brief the user, keeping it to about fifteen lines. The user is familiar with the work of the arc. Tell them what changed since the last handoff and where this session is headed, not the details of what each file says.

- Where the work stands, in a sentence or two.
- The `next` action, and the workflow governing it.
- Discrepancies from step 4.
- Unconfirmed proposals that `next` depends on.
- Open questions, specifically those that block `next` from proceeding.
- Open review items, briefly.

## Step 6: Either wait or begin

Without `go`, stop after the brief. The user will direct you.

With `go`, the session is unattended. Begin the `next` action under its workflow unless one of these holds, in which case brief and stop:

- An open question blocks `next`. Present the options and the evidence recorded in the arc's files, but do not settle it yourself.
- The `next` action depends on an unconfirmed proposal.
- A discrepancy needs the user's call, including lint errors and missing references.
- A pending entry exists.
- `next` would need a different workflow, or a change to the current one. An unattended session leaves the workflow as it is (see *Decisions and provenance* in HANDOFF.md).
