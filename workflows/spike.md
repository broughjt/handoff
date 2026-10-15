# `spike`

An investigation with a budget. The deliverable is a written verdict, not
working software.

Agree the budget with the user before starting, whether that is a number of
hours, a single session, or something else they name. When it is spent, stop
and report, including when the question is still open. Report the state of the
evidence rather than a guess.

Experiments are committed on a new branch that is not intended to be
merged.

The verdict goes in `arc:spike-<slug>.md`. This should consist of the
question, what was tried, what was observed with real numbers, the name of the
throwaway branch, and a recommendation. A `read-first` entry in `start.toml`
points at that file, and a topic document states the conclusion itself, so the finding
outlives the file.

The spike ends when the investigation yields answers. Building on it is a
separate task under a different workflow, so the design decisions get made
deliberately instead of inherited from throwaway code.

## Review

None. Nothing is added to `review.md` for the whole spike.
