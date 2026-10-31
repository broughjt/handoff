---
name: handoff-create
description: Begin an arc, a goal pursued over multiple sessions, in the project's handoff repository. Use when the user asks you to create a handoff arc or when a task is clearly too big to finish in one session. Companion skills are handoff-start and handoff-update.
---

# Create a handoff arc

## Step 1: Read HANDOFF.md

Run `handoff guide` and read the file at the path it prints, unless you have already read it this session and it is still in view. Reread it after compaction or whenever you are unsure of a rule.

## Step 2: Triage

Decide whether the proposed work fits into an arc. Ask the user how long they expect the work to take if it is not obvious.

- Plausibly one session: not an arc. Say so and offer to do the work.
- Several small independent items: not an arc either, and not one each. They are just work; offer to do them.
- Multiple sessions toward one goal: this is what constitutes an arc.

If the user overrides your triage, follow them without re-arguing the point.

## Step 3: Infer before you ask

Never ask about what you can observe. Read until you know what the project is, which commands can verify claims about it, which files fresh agents will need for this work, and what other arcs, if there are any, have already settled.

- `README.md`, `docs/` directories, and other documentation in the repository.
- `AGENTS.md` or `CLAUDE.md` files.
- Other arcs, listed by `handoff list`: their `arc.md`, `do-not-reopen.md` and workflow copies.
- Topic documents at the root of the handoff repository.
- Build and test configuration.
- The last 10 to 20 commits and `git status`.

Then try the commands you expect to become checks, and time them, so the checks and their timeouts are real before anyone relies on them. A failure at this point is useful information about where the arc begins; raise it during the interview.

## Step 4: Interview

Interview the user about what you could not infer, using the structured question tool (if it exists, otherwise directly in chat). Choose questions based on what step 3 left open. The following are example topics, not an agenda:

- Motivation: why the work exists, and what is inadequate today.
- Done condition: what the finished state looks like, concretely enough to check.
- Route: the overview of the work to accomplish the goal. This is not a detailed task list.
- Workflow: see below.
- Do not reopen: what has been decided, tried and rejected, or ruled out of scope. This is the most valuable section and users often forget to volunteer it, so ask directly.
- Open questions: decisions that should be reserved for the user to make.

Interview until you are 95% confident you are on the same page. Do not write the arc while substantive questions are open.

### Choosing a workflow

A workflow settles how the user wants to work with you on this arc (see *Workflows* in HANDOFF.md), so treat this as part of the interview. Find out what role the user wants you to play and where their judgment should come into the work.

Start from what exists. `handoff workflow list` shows the workflows the user keeps for reuse, and other arcs may run against workflows of their own. Open the ones that could fit, and propose the closest. Propose a variation or a new workflow only when you can name something about this arc that the existing workflows handle badly. For example, an interface that has to be settled before anything else, a check too slow to run every session, or a component only the user can exercise. If you cannot name one, use the closest existing workflow. The arc can switch later, once a mismatch shows.

The workflow only needs to fit the arc's first phase. When it fits only that phase, say so in the route in `arc.md`.

## Step 5: Write

If `.scratch/handoff/` does not exist, ask the user to create it. Do not run `git init` yourself unless the user gives you explicit permission.

Run `handoff new <arc> [<workflow>]`, passing the workflow chosen in the interview:

- An existing workflow used as is: specify it, and `handoff new` will install a verbatim copy and set `workflow`.
- A variation: run `handoff new <arc>`, then `handoff workflow fork <arc> <source> [<name>]`, amend the fork, and set `workflow` to the reference it prints.
- A new workflow: run `handoff new <arc>`, write `arc:workflow-<name>.md` following *Writing a workflow* in HANDOFF.md, opening with `<!-- created for this arc on <date> -->`, and set `workflow` to it.

Then fill in every file:

- `arc.md`: motivation, done-when and route.
- `tasks.md`, `do-not-reopen.md`, `open-questions.md` from the interview.
- `checks.toml`: replace the placeholder with real checks, cheapest catch-all first. Run `handoff check` until each one executes as intended. False check outcomes are okay, especially at the beginning of an arc.
- Topic documents: facts you learned in step 3 that a later session would otherwise have to rediscover, recorded as *Storing findings* in HANDOFF.md describes.
- `start.toml` last, once everything else is settled, replacing the placeholder `next` and the placeholder `workflow` if `handoff new` was not passed one.

Decisions from the interview get user tags. Anything you inferred and the user did not confirm is still an agent proposal.

Run `handoff lint` until it is clean.

## Step 6: Record the first handoff

Follow handoff-update from its step 3. The log entry should record the arc's creation and the decisions made in the interview. Skip handoff-update's confirmation step, since the interview here was the same thing.
