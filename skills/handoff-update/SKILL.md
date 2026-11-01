---
name: handoff-update
description: Record a handoff so future sessions can resume from it. Runs the checks, writes the log entry, edits the arc's state in place, refreshes `start.toml` for the next session, confirms decisions with the user, and commits. Use when the user asks you to record a handoff or at the end of a session that began with handoff-start. Companion skills are handoff-create and handoff-start.
---

# Record a handoff

Arguments:

- none: update the only arc
- `<arc>`: update that arc
- `close`, `<arc> close`: close the arc

## Step 1: Read HANDOFF.md

Run `handoff guide` and read the file at the path it prints, unless you have already read it this session and it is still in view. Reread it after compaction or whenever you are unsure of a rule.

## Step 2: Settle project state and `checks.toml`

The `handoff update --commit` command refuses if `checks.toml` or the project's branch or head changed after `--begin`, so do both of these first:

- Commit the session's project work.
- Finalize `checks.toml`: add checks for what this session established, add checks that are currently false for lasting state that still needs fixing ("the old API key is still live"), and fix exit code squashes that produced an unknown.

## Step 3: Create the new log entry

Run `handoff update --begin [arc]`. If the linter refuses, fix the files it complains about and rerun. Read the results it prints before writing anything. The log entry you write should explain them.

## Step 4: Write `entry.md`

Write `entry.md` at the path `handoff update --begin` printed. Open it with a heading that says what changed, not "worked on X". This heading will become the commit subject later, prefixed with `<arc>: `.

Include:

- What was done, and what was observed outside the checks, with real numbers.
- Deviations from the plan, each with its argument. This is the most valuable content in the log entry.
- Significant check results explained: a check that flipped, an unknown, or a check that is still false. Do not copy the check results in `results.toml`, since they are shown in the report from `handoff start`.
- Actions that were not taken or commands not run in this session.
- Decisions made this session, with provenance, and any changes to workflows with their reasons.
- Items removed from `review.md`, with quotes from the user.

A fact that stays true beyond this session is not an observation. Record it in a topic document (step 5), and point to it from the log entry rather than restating it there.

## Step 5: Edit state in place

Update the following in place:

- `tasks.md`: status, with the date and the commit subject for finished tasks.
- Discovered facts: add to a topic document, either at the root or arc-local, as described in *Storing findings* in HANDOFF.md. Correct inaccurate facts in place, and split out topic documents that have grown beyond a single subject.
- `do-not-reopen.md` and `open-questions.md`: follow *Decisions and provenance* in HANDOFF.md. Delete answered questions and move their answers to the appropriate locations.
- `review.md`: add an item for work that ends unreviewed, following the workflow's extra guidance.
- `arc.md`: when the arc's goal, route, or done-when condition has changed.
- Task documents whose task closed: check that no durable findings are left over, then remove them with `git rm`.

## Step 6: Refresh `start.toml`

You are in the best position to know what the next session will need to pick the work back up. Rewrite every field of `start.toml` for the next session with this in mind:

- `next`: one concrete action, beginnable cold.
- `read-first`: files the next agent should read, each with a reason. Drop task documents when their task has closed. This is the field that saves the next session the most time and the one most often left stale.
- `workflow`: the copy governing `next`. To switch, run `handoff workflow install` or `handoff workflow fork`, then set the field to the reference it prints. In an unattended session, leave the workflow as it is.

## Step 7: Confirm decisions

Skip this step in an unattended session (begun with `handoff-start go`) or when there is nothing to ask.

Otherwise, interview the user with the structured question tool (if it exists, otherwise directly in chat), covering:

- every line you are recording with a user tag, so the user can check your reading of what they said
- every new proposal `next` depends on
- every check newly specified as a task's done-when
- every workflow change: adopting, creating, or switching a workflow, or changing user or agent roles

If the session showed friction that a different way of working could remove (see *Workflows* in HANDOFF.md), and you have not raised it already, propose a change to the workflow in this round.

Answers become user tags. Refusals become edits to your log entry and the arc files. Anything left unanswered by the user stays `(agent proposal, date, unconfirmed)`. If the user has left partway through, record the proposals as unconfirmed rather than waiting.

## Step 8: Commit

Stage the root files this update changed (topic documents, shared workflow copies) with `git -C .scratch/handoff add`. Then run `handoff update --commit [arc]`.

If the command refuses, follow the direction it gives. Usually this means rerunning `update --begin` (which preserves the content in `entry.md` you wrote) after fixing something. Reread the new check results, and amend the entry if they change the narrative.

The command lists what it left uncommitted; mention these to the user.

## Step 9: Brief the user

Brief the user about what changed in the arc, what you promoted or removed, and any claim you could not verify. Keep it to 15-20 lines.

## Closing an arc

Only when the user says the arc is closed.

1. Check that everything that outlives the arc is already in root topic documents, and move anything that is not.
2. Run steps 2 through 9. Your log entry should say how the arc ended.
3. Run `handoff close <arc>`. It tags the commit `closed/<arc>`, which keeps the arc's final state addressable, then removes the arc's directory and commits. If it refuses, follow the direction it gives.
