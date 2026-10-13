---
name: handoff-create
description: Begin an arc, a goal pursued over multiple sessions, in the project's handoff repository. Use when starting a sustained piece of work, when the user asks for a handoff, or when a task is clearly too big to finish in one session. Companion skills are handoff-start (resume an arc) and handoff-update (record a handoff).
---

# Beginning an arc

## Step 1: Read HANDOFF.md

Run `handoff guide` and read the file it names, unless you have already read it
this session and it is still in view.

## Step 2: Triage

Decide whether this is an arc. Ask the user how long they expect the work to
take if it is not obvious.

- **Plausibly one session**: not an arc. Say so and offer to do the work.
- **Several small independent items**: not an arc either, and not one each.
  They are just work; offer to do them.
- **Multiple sessions toward one goal**: an arc.

If the user overrides your triage, follow them without re-arguing.

## Step 3: Infer before you ask

Never ask about what you can observe. Read until you know what the project is,
what verifies it, which files a fresh agent needs for this work, and what
sibling arcs already settled.

- `README.md`, `AGENTS.md` or `CLAUDE.md`, and other public documentation.
- `handoff list`, sibling arcs' `arc.md` and `do-not-reopen.md`, and the root
  topic documents.
- `handoff workflow list`.
- Build and test configuration.
- The last 10 to 20 commits, and `git status`.

Then try the commands you expect to become checks, and time them, so the
checks are real before anyone relies on them. A failure is a fact about where
the arc starts; raise it in the interview.

## Step 4: Interview

Ask about what you could not infer, with the structured question tool,
recommendation first and labeled `(Recommended)`. Choose the questions from
what step 3 left open. These are examples, not an agenda:

- **Motivation**: why the work exists, and what is inadequate today.
- **Done**: what the finished state looks like, concretely enough to check,
  and which checks would show it.
- **Route**: the shape of the path, not a task list. Phases, ordering
  constraints, what must be settled before what.
- **Workflow**: see below.
- **Do not reopen**: what has been decided, tried and rejected, or ruled out
  of scope. This is the most valuable section and users rarely volunteer it,
  so ask directly.
- **Open questions**: decisions the user is reserving.

Interview until you are 95% confident you are on the same page. Do not write
the arc while material questions are open.

### Choosing a workflow

`handoff workflow list` is a starting vocabulary, not a menu. Shortlist from
its summaries, open the candidates you are weighing, and propose the workflow
that suits this arc, including one the user has not thought of.

Propose a variation only when you can name the property of this arc that the
patterns handle badly: an interface that has to be pinned before anything
else, a check too slow to run every session, a component only the user can
exercise. If you cannot name one, pick the closest pattern.

A coined workflow needs a short name and a definition a fresh agent can follow
without asking: what the agent does, in what order, what it does not do, what
counts as finished, and what goes on the review list.

A workflow may fit only the phase in front of you. When it does, say so.

## Step 5: Write

If `.scratch/handoff/` does not exist, ask the user to create it. Do not run
`git init` yourself.

Run `handoff new <arc> [<pattern>]`, or fork or coin the workflow afterwards
and set `workflow` yourself. Then fill in every file:

- `arc.md`: motivation, done-when and route.
- `tasks.md`, `do-not-reopen.md`, `open-questions.md` from the interview.
- `checks.toml`: replace the placeholder with real checks, cheapest catch-all
  first. Run `handoff check` until each one executes as intended. A false
  outcome is fine; it is where the arc starts.
- `start.toml` last, once everything else is settled.

Decisions from the interview are `(user, date)`. Anything you inferred and the
user did not confirm is an agent proposal.

Run `handoff lint` until it is clean.

## Step 6: Record the first handoff

Follow handoff-update from its step 3. The entry records the arc's creation and
the decisions taken in the interview. Skip handoff-update's confirmation step:
the interview was it.
