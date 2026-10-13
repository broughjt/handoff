# Handoff

Sustained work outlives a session, and the agent that picks it up next starts
with no memory of what came before. Handoff is the practice of carrying that
work across the gap. The work is an **arc**: one goal pursued over many
sessions, with its state kept in files. A session passes the arc on by making
a **handoff**, which an update records. The next session begins by reading the
latest handoff back.

The `handoff` script does the mechanical work: it finds files, runs checks,
compares the present against the last handoff, and reads Git state. It prints
what it finds. Agents do the judgment: what a discrepancy means, what to do
next, what to write down. When the script's output and some prose claim the
same mechanical fact, trust the script.

The user rarely reads these files. They read what you tell them in chat. So
nothing in an arc is settled until the user has said so, and the procedures
below exist to put the right questions in front of them (see *Decisions and
provenance*).

## The handoff repository

Arcs live in `.scratch/handoff/`, a Git repository of its own inside the
project. The project ignores `.scratch/`, so `rg --files`, `fd` and the
project's Git listings omit it; reach it by path. The script finds it by
walking up from the current directory, so it works from anywhere in the
project or inside the handoff repository itself.

- The user creates the repository (`git init .scratch/handoff`). Agents never
  do. If it is missing, say so and stop.
- Handoff files are committed in the handoff repository, never in the
  project. Use `git -C .scratch/handoff` for anything the script does not do.
- **Synchronization is the user's.** Never push or pull, in either
  repository, unless the user asks.

Paths in this document and in the skills are written from the project root.

## Arcs

An arc is a top-level directory of the handoff repository that contains
`start.toml`. Files at the root of the repository are shared: topic documents
and verbatim workflow copies.

| File | Holds | Written by |
|---|---|---|
| `start.toml` | `next`, `workflow` and `read_first`: the interface to the next session | agent |
| `arc.md` | why the work exists, what done looks like, and the route there | agent |
| `tasks.md` | tasks and their status | agent |
| `do-not-reopen.md` | settled scope and rejected approaches, each with its reason | agent, from the user's words |
| `open-questions.md` | decisions reserved for the user | agent |
| `review.md` | work the user has not reviewed | agent; only the user's words remove an item |
| `checks.toml` | the arc's checks | agent |
| `log/<N>/entry.md` | the narrative of one handoff | agent; immutable once committed |
| `log/<N>/results.toml` | the checks' results at that handoff | script |
| anything else | arc-local topic documents, forked and coined workflows, task documents | agent |

**Never edit a file the script writes.** Edit everything else directly with
your ordinary file tools; there are no commands for editing state.

Every file in the layout always exists. An empty section means "not yet", not
"missing".

`start.toml` looks like this:

```toml
next = """
Implement the parser's error recovery from arc:plan-recovery.md, \
starting with the unterminated-string case."""
workflow = "handoff:workflow-agent-implements.md"

[[read_first]]
path = "arc:plan-recovery.md"
reason = "the plan this task implements"

[[read_first]]
path = "project:src/parse.rs"
reason = "the recovery hooks go in Parser::next_token"
```

An arc is for multiple sessions toward one goal. Work that fits in one session
is just work, and several small independent items are not an arc either.
**There is no backlog.** When the user mentions future work that is not
arc-sized, leave it with them. Never open a TODO file, a queue or a "future
work" section to hold it.

### References

Files are named by a scope and a relative path without `..`:

- `arc:<path>`: inside the arc's directory.
- `handoff:<path>`: from the root of the handoff repository.
- `project:<path>`: from the root of the project.

`start.toml` must use them, and lint checks that every file they name exists.
Use the same form in prose wherever you point at a file.

## Checks

Checks are claims about the world that a command can decide. They replace
hand-written verified state: an agent writing "the build is green" from
memory gets it wrong, and a command does not.

Each check in `checks.toml` is a `[[check]]` table, run in file order:

```toml
[[check]]
id = "tests"
claim = "The test suite passes"
run = "cargo test --quiet"
timeout = 300
```

`id` names the check across handoffs, `claim` says in natural language what a
true result means, and `run` is a shell command. `timeout` is in seconds and
defaults to 60.

**Outcomes.** Exit 0 means true, exit 1 means false, running past the timeout
is a timeout, and any other exit code is unknown. Squash a command's exit
codes into that convention:

- `test -e path || exit 1`, since `ls` and friends exit 2 for a missing path.
- `systemctl is-active --quiet unit || exit 1`, since inactive is exit 3.
- `ssh host 'cmd || exit 1'`: squash on the remote side, so that ssh's own
  255 still means the connection failed (unknown) rather than false.
- `sudo -n true 2>/dev/null || exit 3; sudo -n cmd`, so that a check needing
  root reports unknown instead of hanging or lying.

**Checks are read-only, always.** They observe and never change anything.
They run from the project root, whatever directory `handoff` was run from.

**A false result is an observation, not a failure of the handoff.** An update
may record checks that are false. A timeout means unverified, not
contradicted. An unknown is a fact about the world or a check that needs
better squashing; find out which.

A **lasting negative** ("the old API key is still live", "the migration has
not run on the replica") is a check that is currently false. It reports itself
at every start and flips when someone fixes it.

`handoff check` runs the checks at any time without recording anything.

`handoff new` writes a placeholder check that lint rejects. Replace it with at
least one real check before the arc's first update. The first check belongs to
the cheapest one most likely to notice the arc going stale.

Naming a check as a task's done-when is a decision (see *Decisions and
provenance*).

## Updates

An update records a handoff. It is not the end of a conversation: the user may
ask for one mid-session, and a session may make several. Running checks
routinely never makes one.

An update is two commands with your writing in between:

1. `handoff update --begin [arc]` lints the arc, refusing on any error, runs
   the checks, records `results.toml` in the pending entry `log/<N>/`, and
   prints the results and the path of `entry.md`.
2. You write `log/<N>/entry.md` and edit the arc's state.
3. `handoff update --commit [arc]` validates, then commits the entry, the
   results, the whole arc directory and whatever you staged, as
   `<arc>: <headline>`, where the headline is the entry's opening heading.

**Pending** means the highest-numbered `log/<N>/` is not in the handoff
repository's `HEAD`. A pending entry may be edited; a committed one is
immutable. Rerunning `--begin` replaces the results and keeps `entry.md`.

`--commit` refuses, changing nothing, if `checks.toml` changed or the project's
branch or head moved since `--begin`. Rerun `--begin` and look at the new
results before committing. So commit your project work, and settle
`checks.toml`, before `--begin`.

The next start compares its own run of the checks against the latest committed
entry's results, and inlines that entry's `entry.md`. **The entry is what the
next agent reads about this session.**

## Decisions and provenance

Agent-written plans harden into instructions when nobody checks them. So every
decision carries who made it:

- `(user, YYYY-MM-DD)`: the user said it, on that date.
- `(agent proposal, YYYY-MM-DD, unconfirmed)`: an agent proposed it, and the
  user has not agreed.

Tag the route and the done-when in `arc.md`, tasks, do-not-reopen entries,
open questions (who raised or deferred each one), and a `next` that follows
from a decision. Do not tag facts in topic documents, log entries or check
claims; those are observations, not decisions.

**Only the user's words settle anything.** You may propose anything, tagged as
a proposal. You may not close an open question, add a settled entry to
do-not-reopen, or confirm a proposal unless you can point at what the user
said. Confirming rewrites the tag with the date the user said it.

Mark an open question `— blocks Next.` when the action in `next` cannot
proceed without it, and remove the marker when that stops being true. The
agent that writes `next` is the only one in a position to judge this; the
next one takes the marker at face value.

**Unattended sessions.** A session begun with `handoff-start go` is unattended.
Its update skips the confirmation interview and records every new decision as
unconfirmed. The next `go` session then stops if `next` rests on one of those
proposals, so a run of unattended sessions cannot drift far from what the user
agreed to. That is intended.

## The review list

`review.md` lists work the user has not reviewed. Each item names its unit by
content and gives the date it was added:

```markdown
- The parser error-recovery commits, from "Recover from unterminated strings"
  to "Test recovery at end of input" on `parse-recovery` (added 2026-10-13)
- The findings in arc:review-lexer.md (added 2026-10-11)
```

- Never name a unit by commit SHA. The user rewrites history, and a SHA stops
  resolving within the hour.
- Add an item at update when work ends unreviewed. The workflow says which
  unit to add.
- **Only the user's words in chat remove an item**, and the entry that removes
  it cites them.
- The list never holds `next`. Work goes on while a review is open. A workflow
  may gate its own stages on the list; `design-first-tdd` does.

## Where facts go

A finding goes in the arc's files only if future work would otherwise have to
re-derive it. Ask: would it hold if this arc had never existed?

- **Yes**: a root topic document, `handoff:<topic>.md`, which outlives the
  arc.
- **No**: an arc-local topic document, `arc:<topic>.md`, removed with the arc.

A fact has one home. Everywhere else, point at it by reference. When a fact
changes, correct it where it lives.

`read_first` names topic documents when `next` needs them. `handoff start`
lists every arc file outside the layout, so a document no entry names is still
known to exist.

**Task documents** (`arc:plan-<task>.md`, `arc:spike-<slug>.md`,
`arc:review-<slug>.md`) live in the arc, are named in `read_first` while their
task is live, and are removed with `git rm` when it closes, after anything
durable in them has moved to a topic document. Git history keeps them.

Promotion into the project's own documentation (AGENTS.md, a README, `docs/`)
is the user's alone. Never move text there yourself, and do not mark text as a
candidate for it.

## Workflows

A workflow says how work on an arc proceeds: who writes what, what the agent
does not do, what counts as finished, and what goes on the review list.

**Patterns** are the reusable workflows, listed by `handoff workflow list`.
They live outside the repository and are never edited from a session. If a
change would be generally useful, suggest it to the user.

**An arc runs against a copy, and the copy governs.** `start.toml`'s
`workflow` names it. A task may override it, naming its own copy in
`tasks.md`. Each copy opens with a provenance line:

| Line | Written by | Lives |
|---|---|---|
| `<!-- copied from workflow pattern <name> (sha256:…) on <date>; verbatim -->` | `handoff new`, `handoff workflow install` | at the root, shareable between arcs |
| `<!-- forked from <source> (sha256:…) on <date> -->` | `handoff workflow fork` | in the arc |
| `<!-- coined for this arc on <date> -->` | you | in the arc |

The hash is of the source at copy time. It shows later whether a verbatim
copy was amended or its source has moved on.

- **Never amend a verbatim copy**; other arcs may run against it. Fork it with
  `handoff workflow fork <arc> <name> <source>` and amend the fork.
- Amend forked and coined workflows in place. Date the change in the file and
  record it in the update's entry.
- Neither `install` nor `fork` edits `start.toml`. Set `workflow` yourself to
  the reference they print.

A procedure that repeats across sessions goes under a named section of the
arc's workflow copy, and `next` names that section rather than restating it.

## Writing state

- **Edit state in place.** When something changes, change it. Delete
  superseded text instead of annotating it with a correction.
- **Every fact is actionable cold.** "Use the warm state" is useless; "one warm
  `PersistentTCState` shared via `withResource`; never build a second warm-up
  path" is not.
- **No section restates a file it names.** Point at the file and say what to
  look for.
- **Never write a number or result you did not observe.** If you did not run
  it, say what to run.
- **No commit SHAs in state files** (`start.toml`, `arc.md`, the section
  files, topic documents). Name commits by subject and branch. Log entries may
  cite SHAs as history.
- **`next` is one concrete action, beginnable cold**, and `read_first` lists
  only what that action needs, each entry with a reason.

## Commands

Every command takes the arc as an optional last argument when the handoff
repository has exactly one; with several, it refuses and names them.
`--repository <path>` (before the command) uses another handoff repository.

| Command | Does |
|---|---|
| `handoff list [--archived]` | active arcs, or closed ones from their `archive/` tags |
| `handoff start [arc]` | the brief: state inlined, project and handoff repository status, checks compared against the last handoff. Writes nothing |
| `handoff check [arc]` | run the checks without recording them |
| `handoff lint [arc]` | validate the layout, `start.toml`, references, `checks.toml` and log entries |
| `handoff update --begin [arc]` | record fresh results in the pending entry |
| `handoff update --commit [arc]` | commit the pending entry and the arc |
| `handoff new <arc> [<workflow>]` | create an arc's files, uncommitted |
| `handoff workflow list` | the patterns |
| `handoff workflow install <pattern>` | a verbatim copy at the root |
| `handoff workflow fork <arc> <name> <source>` | `arc:workflow-<name>.md`, forked from a pattern or a reference |
| `handoff guide` | the path of this file |

Exit codes: 0 success; 1 when `check` finds a check not true or `lint` finds
errors; 2 for an input error.
