---
name: handoff-start
description: Resume multi-session work from the latest handoff of an arc. Runs `handoff start`, reads what it points at, interprets what changed since the last handoff, and briefs the user on where things stand. Use at the beginning of a session on ongoing work, or whenever the user asks to pick up where things left off. Companion skills are handoff-create (begin an arc) and handoff-update (record a handoff).
---

# Resuming an arc

Arguments:

- *(none)*: find the arc, brief, wait
- `<arc>`: use that arc
- `go`: brief, then begin `next` without waiting
- `<arc> go`: both

## Step 1: Read HANDOFF.md

Run `handoff guide` and read the file it names, unless you have already read it
this session and it is still in view. Reread it after compaction, or whenever
you are unsure of a rule. It defines every term below.

## Step 2: Run `handoff start`

Run `handoff start [arc]` synchronously; it runs the checks, so give it the
time they need. Never put it in the background.

- **Several arcs**: it refuses and names them. Show each one's `next` from its
  `start.toml` and ask which. Do not guess from recency; the user may be
  switching arcs deliberately.
- **No arcs**: say so, offer handoff-create, and stop.
- **No handoff repository**: say so and stop. The user creates it.

## Step 3: Read what the brief points at

The brief inlines `start.toml`, `arc.md`, the workflow, the section files and
the latest entry. Do not read them again.

Read every `read_first` entry the brief marks present. Read other arc files and
topic documents only when `next` needs them. Do not read older log entries
unless something in the brief is unclear and the history would resolve it.

## Step 4: Interpret the report

Report discrepancies. **Never reconcile one silently**: the user decides
whether the documents or the world is wrong.

**Checks**, compared against the last handoff:

- A flip the latest entry does not explain.
- A check that is new, removed, or redefined under the same id.
- Unknown: the check needs better squashing, or the world is not what it
  claims. Say which you think, and why.
- Timeout: unverified, not contradicted.

A false check the latest entry already explains, such as a lasting negative,
is not a discrepancy.

**Project repository:**

- Commits since the recorded head are work done outside a session. When the
  user wrote them, they are this session's first input: review them before
  `next`.
- Rewritten history: map the subjects named in the latest entry and in
  `review.md` onto the new commits.
- A recorded head that no longer resolves: history was rewritten and pruned,
  or the last handoff was made on another machine.
- A branch that differs from the one recorded.

**Handoff repository:** lint errors, uncommitted changes, and `read_first`
entries or a workflow marked missing.

**A pending entry** was begun and never committed. Report its host and time and
whether it has `entry.md`, and ask the user whether to finish it (rerun
`handoff update --begin`, review the entry, `--commit`) or discard it. Never
finish or delete it unasked.

**Review items whose work appears to have landed, been rewritten, or vanished**:
raise each as a question. Never remove one.

## Step 5: Brief

Keep it under about fifteen lines. The user knows the arc; tell them what
changed since the last handoff and where this session is headed, not what the
documents say.

- Where the work stands, in a sentence or two.
- `next`, and the workflow governing it.
- Discrepancies from step 4.
- Unconfirmed proposals `next` rests on. List them every session until the
  user confirms or strikes them.
- Open questions, those marked `— blocks Next.` first.
- Open review items, briefly. They never hold `next`.

## Step 6: Wait, or begin

Without `go`, stop after the brief. The user will direct you.

With `go`, the session is unattended. Begin `next` under its workflow unless
one of these holds, in which case brief and stop:

- An open question blocks `next`. Present the options and the evidence the arc
  holds, but do not settle it.
- `next` rests on an unconfirmed proposal.
- A discrepancy needs the user's call, including lint errors and missing
  references.
- A pending entry exists.
