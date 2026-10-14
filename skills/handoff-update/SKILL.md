---
name: handoff-update
description: >-
  Record a handoff for an arc: run the checks, write the log entry, edit the
  arc's state in place, refresh start.toml for the next session, confirm
  decisions with the user, and commit. Use when the user asks for an update,
  at the end of a session on multi-session work, or with `close` to close an
  arc. Companion skills are handoff-create and handoff-start.
---

TODO: handoff-update has no step for proposing a workflow change mid-arc. Step 4 records a switch, but nothing prompts the agent to suggest one. One sentence in step 7's confirmation interview would cover it: a proposed workflow change is a decision to confirm there.

# Recording a handoff

Arguments:

- *(none)*: update the only arc
- `<arc>`: update that arc
- `close`, `<arc> close`: close the arc, only when the user has said to

## Step 1: Read HANDOFF.md

Run `handoff guide` and read the file it names, unless you have already read it
this session and it is still in view. Its *Updates*, *Decisions and
provenance* and *Writing arc files* sections govern everything below.

## Step 2: Settle the inputs

`--commit` refuses if `checks.toml` or the project's branch or head changed
after `--begin`, so do both first:

- Commit the session's project work.
- Edit `checks.toml`: add checks for what this session established, turn
  lasting negatives into checks that are currently false, fix squashing that
  produced an unknown.

## Step 3: Begin

Run `handoff update --begin [arc]`. If lint refuses, fix the files it names
and rerun. Read the results it prints before writing anything; the entry
explains them.

## Step 4: Write the entry

Write `entry.md` at the path `--begin` printed. Open it with a heading that
says what changed, not "worked on X"; it becomes the commit subject.

- What was done, and what was observed outside the checks, with real numbers.
- **Deviations from the plan, each with its argument.** This is the most
  valuable content in the entry: it is the reviewer's brief, and the only part
  of what they read that you write. They already have the plan and the diff.
- Significant results explained: a flip, an unknown, a check still false. Do
  not copy the results; the next start shows them.
- What was not run or not done this session.
- Decisions made this session, with provenance, and any change of workflow
  with its reason.
- Items removed from `review.md`, citing the user's words.

**Do not judge your own code.** Flag what you are unsure of or think deserves
attention, but leave verdicts on its quality to the reviewer unless the user
asks for yours.

## Step 5: Edit state in place

- `tasks.md`: status, with the date and the commit subject for finished tasks.
- Facts: to a topic document, root or arc-local as *Storing findings*
  says. Correct wrong ones where they live.
- `do-not-reopen.md` and `open-questions.md`: only as *Decisions and
  provenance* allows. Delete an answered question and move its answer where it
  belongs.
- `review.md`: add an item for work that ends unreviewed, by the workflow's
  rule.
- `arc.md`: when the route or done-when changed.
- Task documents whose task closed: promote what is durable, then `git rm`
  them.

## Step 6: Refresh `start.toml`

Rewrite every field for the next session, which knows nothing:

- `next`: one concrete action, beginnable cold.
- `read_first`: what that action needs, each with a reason. Drop entries whose
  task closed. This is the field that saves the next session the most time
  and the one most often left stale.
- `workflow`: the copy governing `next`. To switch, run `handoff workflow
  install` or `fork`, then set the field to the reference it prints.

## Step 7: Confirm decisions

Skip this step in an unattended session (begun with `handoff-start go`), and
when there is nothing to ask.

Otherwise ask one round with the structured question tool, recommendation
first and labeled `(Recommended)`, covering:

- every line you are recording as `(user, …)`, so the user can check your
  reading of what they said;
- every new proposal `next` rests on;
- every check newly named as a task's done-when.

Answers become `(user, date)` tags; refusals become edits; anything left
unanswered stays `(agent proposal, date, unconfirmed)`. If the user is gone,
record the proposals as unconfirmed rather than waiting.

## Step 8: Commit

Stage the root files this update changed (topic documents, shared workflow
copies) with `git -C .scratch/handoff add`. Then run `handoff update --commit
[arc]`.

If it refuses, follow what it says; usually that is rerunning `--begin`, which
keeps the entry. Reread the new results, and amend the entry if they change
its story.

It lists what it left uncommitted; mention that to the user. Do not push.

## Step 9: Report

Tell the user what changed in the arc, what you promoted or removed, and any
claim you could not verify, which stays marked rather than quietly dropped.

## Closing an arc

Only when the user says the arc is closed.

1. Move everything that outlives the arc to root topic documents. Check that
   no other arc's `read_first` points into this one.
2. Run steps 2 through 9. The entry says how the arc ended.
3. Tag the commit: `git -C .scratch/handoff tag archive/<arc>`. The tag keeps
   the arc's final state addressable.
4. `git -C .scratch/handoff rm -r <arc>`, then `git -C .scratch/handoff
   commit -m "<arc>: close"`. Remove a verbatim workflow copy at the root in
   the same commit when no other arc names it.
