# Handoff

The handoff skills and the `handoff` script help to coordinate coding agent work across many sessions. An agent starting in a fresh session has no memory of previous ones. To remedy this, the user and the agent collaboratively maintain the state of each piece of sustained work in persistent files. An **arc** is one goal pursued over many sessions, together with the files used to track its progress.

At a stopping point, usually the end of a session, the agent in the current session passes the arc on to the next session by recording a **handoff**, which consists of a **log entry** describing the session together with the state of the arc's files at that point. The procedure for recording a handoff is called an **update**, and is performed by following the handoff-update skill. The agent in the next session begins with the handoff-start skill, which reads the latest handoff back, including the arc's files, the log entry, and the checks compared against those of the last handoff.

This document is addressed to you, the coding agent reading it. The user is the human you are working with. You will read and write nearly everything in the arc's files; the user will rarely read or edit these files themselves. They direct the arc by talking to you in chat, and they review your work. When you write down a decision or a review in the arc's files, you are recording what they said, not your own judgment (see *Decisions and provenance* and *The review list*). Your responsibility is the work itself, what you learn while doing it, and the plans and proposals you make following the direction the user has agreed to, including the choice of what to do next. The `handoff` script manages the mechanical parts of this process, including locating files, running checks, reading Git state, and recording and committing handoffs.

## The handoff repository

Arcs are kept in the **handoff repository**, `.scratch/handoff/`, which is a Git repository of its own inside the project. The project gitignores `.scratch/`, so `rg`, `fd`, and the project's Git will skip it. Reach it by path directly, and do not take an empty search result as evidence that it or a file it contains is missing. The script locates it by walking up from the current working directory, so it works from anywhere in the project or the handoff repository itself.

The user creates the handoff repository. If it is missing, say so and stop. Create it only if the user gives you explicit permission. Arc files and topic documents are committed in the handoff repository, never in the project itself. Use `git -C .scratch/handoff` for anything the script does not do for you.

## Arcs

An arc is a top-level directory of the handoff repository that contains a `start.toml` file. Files at the root of the handoff repository are shared between arcs (e.g. topic documents and verbatim workflow copies).

Arcs are for tracking one goal across multiple sessions. They are not intended to serve as a full-blown project management system. Specifically, do not treat the handoff system as a system for tracking future work beyond the scope of the arc(s) you are working on. When the user mentions future work outside the current arcs, leave it with them, unless they want to start an arc for it. Do not open a backlog, a queue, or a future work section as part of an arc.

An arc closes only when the user says it is finished. Closing it with handoff-update's `close` argument records a closing log entry, tags that commit `archive/<arc>`, and removes the arc's directory. Before that, move anything that should outlive the arc into root topic documents (see *Storing findings*), since everything left in the arc is removed with it. Git keeps the history: `handoff list --archived` lists closed arcs, and `git -C .scratch/handoff show archive/<arc>:<arc>/<file>` reads a file as it was when the arc closed.

Arc directories are expected to contain the following:

| File | Contains |
|---|---|
| `start.toml` | where the next session should start: `next`, `workflow`, and `read-first` |
| `arc.md` | why the work exists, what done looks like, and the route to get there |
| `tasks.md` | tasks and their status |
| `do-not-reopen.md` | settled scope and rejected approaches, each with a reason |
| `open-questions.md` | decisions reserved for the user |
| `review.md` | work the user has not reviewed |
| `checks.toml` | claims that a command can decide (e.g. the project builds, the tests pass) |
| `log/<N>/entry.md` | the narrative of one handoff |
| `log/<N>/results.toml` | the results of running the checks at the point of the handoff |
| anything else | task documents, arc-local topic documents, and forked or created workflows |

The script generates `log/<N>/results.toml`; never edit it yourself. Everything else above is yours to edit directly with your ordinary file tools (except an already committed `entry.md`; see *Updates*).

References to files in the project or handoff repository are specified with the format `<scope>:<path>`, where `<path>` is a relative path without `..` and `<scope>` is one of:

- `arc:<path>`: inside the arc's directory.
- `handoff:<path>`: from the root of the handoff repository.
- `project:<path>`: from the root of the project.

The `start.toml` format requires this form, and the linter checks that every file it mentions exists. Use the same form whenever you want to reference a file from any of the files in the table above.

### `start.toml`

The `start.toml` file describes where the next session should begin and the context needed to do so. Here is an example `start.toml`:

```toml
next = """
Implement the parser's error recovery following arc:plan-recovery.md,
starting with the unterminated-string case.
"""
workflow = "handoff:workflow-agent-implements.md"

[[read-first]]
path = "arc:plan-recovery.md"
reason = "the plan for this task"

[[read-first]]
path = "project:src/parse.rs"
reason = "the recovery hooks go in `Parser::next_token`"
```

The `next` field describes a single concrete action, written so that an agent with no context can begin it. The `workflow` field specifies the workflow you should follow (see *Workflows*). The `read-first` entries list the files that action needs, and only those, each with a reason. The insight is that the agent recording a handoff is best placed to know what the next session will need, since it has the whole current session in its context. Therefore, agents rewrite every field of this file at every handoff (see *Updates*).

### `arc.md`

The `arc.md` file describes the goal of the arc. It says why the work exists and where it is going. Write the motivation so that someone who has never seen the project understands what is inadequate today and what will have changed when the arc is finished. Then describe what "done" looks like, concretely enough to check, and give a sketch of the route there, including phases and ordering constraints. This route is meant as a general overview, not a task list or a detailed plan. Those go in `tasks.md` or a task document. The `arc.md` file changes rarely. Only edit it when the motivation, destination, or high-level route has actually shifted. The route description and "done" condition should have provenance tags (see *Decisions and provenance*).

### `tasks.md`

The `tasks.md` file contains a list of tasks for the arc, each with a checkbox. Task entries should describe a piece of work concretely enough to begin. Extra detail beyond a few lines of description should go in a task document, and a `read-first` in `start.toml` should be added to reference it when that task is next. Task entries can declare task-local overrides of the workflow by referencing the workflow in their description, e.g. `handoff:workflow-design-first-tdd.md`. When you finish a task, keep the entry in the task list (do not delete it), check its box, and write the date and, if there is one, the subject of the relevant commit. When a task is abandoned, delete its entry. Abandoning a task tagged `user` is itself a decision, so it needs the user's agreement. If a future agent might propose the task again, record why it was dropped in `do-not-reopen.md`. If you discover facts that would be relevant to future sessions while executing a task, add that information to a topic document (see *Storing findings*), not to the task's entry.

Example:

```md
- [x] Parse binary operators — landed 2026-10-09 in "Parse binary operators by precedence" (user, 2026-10-02)
- [ ] Recover from unterminated strings (user, 2026-10-02)
- [ ] Recover from unbalanced brackets, under handoff:workflow-design-first-tdd.md (agent proposal, 2026-10-12, unconfirmed)
```

### `do-not-reopen.md`

The `do-not-reopen.md` file records decisions that future sessions should not relitigate. Fresh agents missing the context from previous sessions often try to helpfully restore purposefully deleted work or re-propose designs that were already rejected. This file prevents this. Check your proposals against it before you make them.

Each entry specifies the decision, together with a reason and a provenance tag. The reason matters as much as the decision itself, since it lets future agents notice when circumstances have changed enough to raise the question with the user again. You may add entries as proposals. An entry becomes settled only when the user explicitly agrees (see *Decisions and provenance*).

Example:

```md
- Recover from errors in the parser, not in a separate pass. A second pass would re-lex the input, and lexing dominates the parse time. (user, 2026-10-02)
```

### `open-questions.md`

The `open-questions.md` file lists decisions reserved for the user. You may gather evidence and lay out the options for a given question, but you may not settle these on your own. Each entry contains the question, things which depend on answering it, and a provenance tag (see *Decisions and provenance*). Tag `user` when the user raised or deferred the question, and `agent proposal` when you raised it and the user has not yet decided one way or the other. When the user answers, delete the entry for the question and move the answer to an appropriate location, usually `do-not-reopen.md`, `tasks.md`, or `arc.md`.

Example:

```md
- Should the parser report every recovery error, or stop after the first? The exit status and the error-format tests depend on it. (user, 2026-10-05)
```

### Other files

Sometimes it is useful for tasks to have associated files, which we call **task documents**. Examples include a plan for the agent executing a task to follow (`arc:plan-<task>.md`), the verdict after running a spike (`arc:spike-<slug>.md`), or findings from a review (`arc:review-<slug>.md`). When a task is current, `start.toml` should list these files as `read-first` entries. Durable facts found during a task belong in topic documents as soon as they are found (see *Storing findings*), so when the task closes, check that nothing durable is left in its task documents, and then remove them with `git rm`.

An arc may also contain arc-local topic documents (see *Storing findings*) and forked or created workflows (see *Workflows*).

## Checks

Checks are claims about the world that can be decided by running a command. Each check in the `checks.toml` file is a `[[check]]` table:

```toml
[[check]]
id = "tests"
claim = "The test suite passes"
command = "cargo test --quiet"
timeout = 300
```

The `id` field gives a persistent identifier to the check across handoffs, `claim` gives a natural language description of what a `true` result means, `command` is a shell command encoding the claim, and `timeout` is measured in seconds and defaults to 60. Check commands are executed from the project root, regardless of where the script was invoked. The checks run in parallel and are reported in file order, so a check's timeout is wall-clock time spent alongside the other checks.

The possible outcomes for a claim are true (exit code 0), false (exit code 1), timeout (command runs past the timeout), and unknown (any other exit code). Your commands should squash exit codes to match this convention. For example:

- `test -e path || exit 1`, since `ls` and friends exit 2 for a missing path.
- `systemctl is-active --quiet unit || exit 1`, since inactive is exit 3.
- `ssh host 'cmd || exit 1'`: squash on the remote side, so that the ssh exit 255 still means the connection failed (unknown) rather than false.
- `sudo -n true 2>/dev/null || exit 3; sudo -n cmd`, so that a check needing root reports unknown instead of hanging or lying.

Check commands should be read-only; they should never modify state.

It is okay for checks to return false, and this does not necessarily indicate a failure of the previous handoff. For example, we might want to check that "the old API has not yet been deleted" or "the migration has not run on the replica", and these checks might expectedly fail for many handoffs. Interpreting the meaning of check outcomes is something that requires agent judgment.

The `handoff check` subcommand runs the checks and prints the results. The `handoff update --begin` subcommand runs the checks and records the results in the new pending log entry (see *Updates*).

A new arc's `checks.toml`, as written by `handoff new`, holds a placeholder check that the linter rejects, so an arc needs at least one real check before its first update.

## Updates

An update records a handoff. It usually happens at the end of a session, but it does not have to. The user might ask you to record one partway through a session, at a useful stopping point.

To perform an update, follow these steps:

1. Execute `handoff update --begin [arc]`. This runs the linter on the arc, refusing on errors; runs the checks, recording the results into `results.toml` in the pending log entry `log/<N>/`; and prints the check results and the path of `entry.md`.
2. You write a log entry in `log/<N>/entry.md` and update the arc's state.
3. Execute `handoff update --commit [arc]`. This runs validation and then commits the whole arc directory, including the log entry, together with anything you staged, with the subject `<arc>: <headline>`, where the headline is the log entry's opening heading. Files outside the arc that the update changed, such as root topic documents or a verbatim workflow copy, are committed only if you stage them first with `git -C .scratch/handoff add`.

A log entry is **pending** if it is the highest-numbered `log/<N>/` and is not in the handoff repository's `HEAD`. Pending log entries may be edited. Rerunning `handoff update --begin` replaces the results but preserves `entry.md`. Once a log entry is committed though, it is immutable and you must not modify it.

Make sure to commit your project work and settle the contents of `checks.toml` before running `handoff update --begin`. If `checks.toml` or the project's branch or head have changed since running `handoff update --begin`, the `handoff update --commit` command refuses, making no modifications.

The content of `entry.md` is then read by the agent in the next session as a description of what happened in the last session. The script also runs the checks and compares the outputs to those in `results.toml` from the last committed log entry.

## Decisions and provenance

It is critical that you make a sharp distinction between your own proposals and the user's decisions. When you write down a decision by itself, nothing in the text says whether the user made it or you proposed it. The next agent, who only reads the files, cannot tell them apart and will treat both with the same weight, causing your proposals to solidify into instructions the user never agreed to. So every decision you write down needs to carry a **provenance tag** indicating who made it:

- `(user, YYYY-MM-DD)`: the user stated or agreed to it, on that date.
- `(agent proposal, YYYY-MM-DD, unconfirmed)`: an agent proposed it, but the user has not yet agreed or disagreed.

Tag the route and done-when condition in `arc.md`, task entries in `tasks.md`, entries in `do-not-reopen.md`, open questions in `open-questions.md`, and the `next` field in `start.toml` when it follows from a decision. Do not tag facts in topic documents, prose in log entries, or claims in `checks.toml` files. These each contain observations, not decisions.

Changing how you and the user work together through workflows is a decision too, although workflow files do not contain provenance tags (see *Workflows*). Only adopt, create, or switch workflows when the user agrees. Record the change, its reasoning, and the user's agreement in the update's log entry.

When reading an arc, treat `user` entries as settled and `agent proposal` entries as open. You may question previous agent proposals.

Important: Only tag `user` when the user stated the decision or unambiguously agreed to it. For example, "yes, let's do it that way" or "we should do option A". Assent only covers what was in front of the user. For example, if they respond "sounds good" to a summary, this does not confirm details you add later, and "go ahead" to starting work does not endorse every choice you make while doing it. When in doubt, keep the `agent proposal` tag and ask the user. You may propose anything you like, just make sure to tag it as `agent proposal`. You may not close an open question, add a settled entry to `do-not-reopen.md`, or confirm a proposal unless you can point to something the user said confirming it. To confirm a proposal, change the tag to `user` with the date they said it. When the user rejects a proposal, delete it, or record it in `do-not-reopen.md` if a future agent might propose it again.

Proposals get confirmed in two places. During each update, the agent interviews the user about every new `user` line and every proposal `next` depends on (handoff-update), and on each start, the agent's brief to the user includes unconfirmed proposals that the `next` task depends on.

A session that begins with `handoff-start go` is **unattended**. Its update skips the confirmation interview and records every new decision as unconfirmed.

## The review list

The `review.md` file lists agent work that the user has not reviewed. Each item specifies its unit of work (e.g. a task, a list of commits, a review findings file) by content and gives the date it was added:

```markdown
- The parser error-recovery commits, from "Recover from unterminated strings" to "Test recovery at end of input" on `parse-recovery` (added 2026-10-13)
- The findings in arc:review-lexer.md (added 2026-10-11)
```

- Never specify a unit by commit SHA, since rebasing rewrites history.
- When work ends unreviewed, add an item during an update. The workflow will declare what the reviewable unit is (or that there is none).
- Only remove an item when the user explicitly indicates that they have reviewed it. Working being finished, rewritten, or removed does not justify removing an item. Ask the user explicitly. The log entry of the update that removes it should quote what they said.
- If the user reviews work in-session, there is no need to add an item here, but do record that they accepted the work in the log entry at update time.
- Items on the list do not block the `next` action in `start.toml`. Work can continue even while items are open. A workflow might add more specific rules here (for example, `design-first-tdd` gates its stages based on the user reviewing in between).

## Storing findings

Over the course of a session you will learn things that later sessions will need. Observations at one point in time ("the push moved 16.9 GB in 7m54s", "no reboot was tried") go in a log entry. Claims that can be decided by a command should become checks in `checks.toml`. Decisions should be recorded in the appropriate arc state file, together with a provenance tag. Standing facts about the project go in **topic documents**, which are Markdown files about one subject that are edited in place as the facts change.

Most of what you learn while working on an arc is about the project, not the arc. The arc is only where you happened to learn it. Those facts stay true after the arc closes, so keep their topic documents at the root of the handoff repository (`handoff:<topic>.md`), where other arcs can find them. The exception is a fact about the arc's own unfinished work, such as "the `parse-recovery` branch still routes errors through the old shim", which stops being true when the arc ends. Keep those in an arc-local topic document (`arc:<topic>.md`), which is deleted with the arc.

When writing topic documents, begin each one with a summary. After the heading, include a first paragraph of prose saying what the document covers. The `handoff start` command lists the arc's other files and the topic documents in the handoff repository root together with their first paragraph summaries, so that an agent can tell which they might need without opening them. The `handoff lint` command flags topic documents that are missing a summary or use lists, code blocks, tables, or HTML instead of prose for the summary. The same goes for task documents and workflow files; their summaries are shown by `handoff start` and `handoff workflow list` in the same way. Verbatim workflow copies at the handoff repository root are not listed.

Record a fact in its topic document when you discover it, or at the latest at the update that follows. Log entries are typically only read once, by the next session, so a fact recorded only there is effectively lost. The entry should say what you found and point to the topic document, not restate it.

When a fact changes, update it in place in the topic document. Topic documents should cover one subject; split them if they grow to cover many.

Whether to promote facts into the project's own documentation (AGENTS.md, a README, `docs/`) is the user's call.

## Workflows

A **workflow** describes how the user would like to work with you during an arc. It covers the roles of the agent and the user, where the user's judgment should affect the work and in what form, conditions every change must meet, what counts as finished, and what goes on the review list.

Workflows are tailored to the work and to the user. Some examples from real arcs:

- For a Haskell library whose interface was the hard part, the user and the agent have a design conversation, encode the design as failing tests, and write a plan that a separate agent, without the design conversation in its context, follows to make the tests pass (`design-first-tdd`).
- For frontend styling, where the user is not a visual designer, the agent builds a panel for adjusting style options interactively in the browser, adds options to it, and interviews the user once they have tried them.
- For a Raspberry Pi's NixOS configuration, every change ends by checking that the installation documentation still describes reality.

You should help discover and propose workflows. You know ways of working, techniques, and tools that the user might not think of or know about. If you notice friction while working on an arc (the user keeps correcting the same kind of thing, iterating on a design is taking too long, a step keeps being forgotten), or think of a more effective way to work, propose a new workflow or a change to the current one in chat. Write it down only once the user agrees (see *Decisions and provenance*).

### Copies

`handoff workflow list` lists the workflows the user keeps for reuse across projects. An arc never runs against these directly. It runs against a copy in the handoff repository, for three reasons: the handoff repository can be synchronized between machines whose configuration may differ, a session must never change the rules for other projects, and the rules an arc started under change only when someone deliberately changes them. Never edit the reusable versions unless the user gives you explicit permission. If a change to a workflow, or a workflow created for an arc, looks generally useful, suggest it to the user.

The `workflow` field in `start.toml` specifies the workflow copy governing `next`, and a task may override it by naming another copy in its `tasks.md` entry. The workflow frequently changes as the arc moves between phases. Record each switch and its reason in the update's log entry.

Each copy opens with a provenance line saying where it came from:

| Line | Written by | Lives |
|---|---|---|
| `<!-- copied from workflow <name> (sha256:…) on <date>; verbatim -->` | `handoff new`, `handoff workflow install` | at the root, shareable between arcs |
| `<!-- forked from <source> (sha256:…) on <date> -->` | `handoff workflow fork` | in the arc |
| `<!-- created for this arc on <date> -->` | you | in the arc |

The hash is of the source at the time it was copied. Hashing the copy shows whether a verbatim copy has been amended, and hashing the source shows whether the source has moved on since.

- Never amend a verbatim copy, since other arcs may run against it. Fork it with `handoff workflow fork <arc> <name> <source>` and amend the fork.
- Amend forked and created workflows in place. Date the change in the file and record it in the update's log entry.
- Neither `install` nor `fork` edits `start.toml`. Set `workflow` yourself to the reference they print.

### Writing a workflow

A workflow written for an arc must be followable by a fresh agent without additional clarification. It should describe what the agent should do and in what order, what it should not do, and what counts as finished. It should also describe what to include in the review list. Put this in a `## Review` section that describes the reviewable unit of work that should be added as an item in `review.md` when work ends unreviewed, or says there are none (see *The review list*). A workflow may fit only a specific phase of the arc. When this is true, say so in the file.

If a procedure is repeated across sessions, indicate this in a named section of the workflow and point the `start.toml` `next` field to that section rather than restating it in `next`.

## Writing arc files

The arc's files are read by agents who lack the context you have, often many sessions after you wrote them. These rules apply to `start.toml`, `arc.md`, `tasks.md`, `do-not-reopen.md`, `open-questions.md`, `review.md`, and topic documents. Log entries are a record of history, so rules 1 and 5 do not apply to them.

1. Edit in place. When something changes, modify the text that describes it, and delete the superseded text instead of annotating it with a correction. The next agent should read only what is true now. The log and Git keep the history when it is needed.
2. Write every fact so that it can be acted on cold. "Be careful with the migration" is useless. "The `migrate.sh` script drops `users.legacy_id`, and nothing else has a copy of that column, so run `pg_dump -t users` first" is useful.
3. Give each piece of information exactly one home. Elsewhere, reference it and say what to look for instead of restating it. Copies drift apart and create bloat.
4. Never write a number or result you did not observe. If you did not run something, say what to run instead.
5. Specify commits by subject and branch, never by SHA. Rebasing rewrites history, so a SHA may later fail to resolve.
6. Write absolute dates. "Yesterday" or "last session" becomes ambiguous as soon as the session ends.

## Commands

This table is a reference. The skills say when to run each command.

Commands shown with `[arc]` take the arc as an optional last argument, which may be left out when the handoff repository has exactly one arc. With several, the command refuses and lists the existing arcs. Passing `--repository <path>` before the command selects a different handoff repository.

| Command | Does |
|---|---|
| `handoff list [--archived]` | list active arcs, or closed ones from their `archive/<arc>` tags |
| `handoff start [arc]` | produce a starting report, including the arc's files inlined, project and handoff repository status, and the checks compared against the last handoff. |
| `handoff check [arc]` | run the checks and print the results |
| `handoff lint [arc]` | validate the layout, `start.toml`, references, `checks.toml` and log entries |
| `handoff update --begin [arc]` | run the checks, record their results in a pending log entry, and print the path of its `entry.md` |
| `handoff update --commit [arc]` | validate and commit the pending log entry, the arc, and anything staged |
| `handoff new <arc> [<workflow>]` | create an arc's files (uncommitted) |
| `handoff workflow list` | list the workflows kept for reuse across projects |
| `handoff workflow install <name>` | make a verbatim copy of a workflow at the handoff repository root |
| `handoff workflow fork <arc> <name> <source>` | fork a reusable workflow or a reference |
| `handoff guide` | output the path of this file |

Exit codes: 0 for success; 1 when `check` finds a check that is not true or `lint` finds errors; 2 for a usage error or a refusal, such as `update --commit` finding stale results.
