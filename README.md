# handoff

This project provides some structure for working with coding agents across many sessions. It consists of three skills and an associated script. I kind of cringe at the term "context engineering", but this basically exists to help you do that, in the limited form of having the agent help you curate context that you can then resume from at the beginning of the next session.

This [blog post](https://mariozechner.at/posts/2026-03-25-thoughts-on-slowing-the-fuck-down/) from the guy who built the [Pi coding agent](https://github.com/earendil-works/pi) is a good proxy for my own sensibilities around software development with coding agents. In short, I value being in the code.

The motivation for this project goes something like this. You value code quality, so you're not one-shotting things and calling it a day. You use the agent as a discussion partner when researching and making design choices, and you have a high tolerance for reviewing and refactoring agent-written code. As a consequence, the stuff you're working on essentially never fits into a single context window. It becomes a giant pain to re-explain your thing at the beginning of each session, so you start having agents write to Markdown files, and now every session begins with "Can you read X, Y, and Z?"

After doing this for several weeks, at some point I thought "hey, having an agent resume from context and having an agent figure out what context to save for the next session both sound like skills." These are the handoff-start and handoff-update skills. Sessions begin with `/handoff-start` and end with `/handoff-update`. There is also a handoff-create skill for helping you create an "arc", which is the term I came up with for a goal pursued over multiple coding agent sessions (see *Usage*). The script automates the mechanical parts of the process that I used to have agents do themselves as a part of following the skills.

## Usage

An arc consists of a directory of Markdown files that the agent maintains at your direction. They include a description of the goal and what done looks like, a next task and a list of files to read at startup, a list of subsequent tasks, decisions not to revisit, open questions reserved for you, and a list of agent work you haven't reviewed yet. The directory is in a "handoff repository" inside a gitignored `.scratch/` directory in your project. That way the agent-generated content is version controlled, but you don't have to commit that strangely worded plan document with an excessive number of colons and em-dashes to your project.

The handoff-update skill writes a log entry for the session, edits the arc's state files mentioned above in place, interviews you about decisions it records (if the session is interactive) and commits. The handoff-start skill tells the agent to run `handoff start`, which collects all that information into a report for it to read. Part of the report compares a list of checks, which are shell commands that test a claim (e.g. the tests pass, the PR is merged), against the recorded output of the checks at the last handoff. The agent then gives you a short brief and waits, unless you run `/handoff-start go`, in which case it starts on the next task without waiting.

Beyond this, the best way to understand how the system works is probably to read [HANDOFF.md](./HANDOFF.md), which is the documentation I wrote for agents to read at the beginning of each session.

## Installation

You need to install both the `handoff` script and the three skills.

If you use Nix, installing is really easy. If you don't use Nix, why aren't you using Nix? It's like [XKCD 353](https://xkcd.com/353/) for package managers. Especially now that you mostly don't have to write Nix by hand anymore (after you've written enough to understand how it works), I don't know why you would use any other package manager. But anyway, if you don't use Nix, you can either install using the Racket package manager `raco` or from the binary release on GitHub.

### With Nix

The flake provides a home-manager module (this requires both home-manager 26.11 or later and nixpkgs 26.11 or later). Add the flake as an input:

```nix
inputs.handoff.url = "github:broughjt/handoff";
# You probably want this too, but it is optional:
inputs.handoff.inputs.nixpkgs.follows = "nixpkgs";
inputs.handoff.inputs.flake-utils.follows = "flake-utils";
```

Then import the module and enable it:

```nix
{ inputs, ... }:

{
  imports = [ inputs.handoff.homeModules.default ];

  programs.handoff.enable = true;
}
```

This puts `handoff` on your `PATH`, adds each skill to `programs.claude-code.skills` and `programs.codex.skills` when Claude Code or Codex is enabled, and links each skill into the `skills` directory of `programs.pi-coding-agent.configDir` when the Pi coding agent is enabled. You can override this with the `programs.handoff.enableClaudeCodeIntegration`, `programs.handoff.enableCodexIntegration` and `programs.handoff.enablePiIntegration` options. For any other agent harness, you should be able to link the skills up yourself by using the `programs.handoff.skills` option.

Without home-manager, there is also a Nix overlay that provides a `pkgs.handoff` package.

### Without Nix

For the skills, if you've got Node, there is a `skills` CLI from Vercel you can use:

```sh
npx skills add broughjt/handoff -g -a claude-code -a codex
```

The `-g` means to install to a global user directory instead of installing locally to the project, and the `-a` option specifies which agent harnesses to target. See <https://github.com/vercel-labs/skills/blob/main/docs/commands.md#skills-use>.

If you don't use Node, copy the `handoff-start`, `handoff-update`, and `handoff-create` directories to `~/.claude/skills` or `~/.agents/skills` or wherever (see <https://code.claude.com/docs/en/claude-directory> or <https://learn.chatgpt.com/docs/build-skills#where-codex-loads-local-skills>).

For the `handoff` command installation, you have two options. You can either (1) install as a Racket package or (2) install from a binary release if you're on Linux.

First option:

```sh
raco pkg install --deps search-auto git+https://github.com/broughjt/handoff
```

Currently, `toml-lib` has a bug that causes it to misread floats such as `0.05`, so the package depends on a fork with the fix until the [upstream PR](https://github.com/toml-racket/toml-racket/pull/24) is merged. If you already had `toml-lib` installed, `raco` keeps that copy, and you'll need to switch it to the fork:

```sh
raco pkg update 'git+https://github.com/broughjt/toml-racket?path=toml-lib#fix-floats'
```

Second option: download the tarball from the [latest release](https://github.com/broughjt/handoff/releases/latest), unpack it somewhere permanent, and link the command onto your `PATH`:

```sh
mkdir -p ~/.local/opt ~/.local/bin
tar -xzf handoff-v*-$(uname -m)-linux.tar.gz -C ~/.local/opt
ln -s ~/.local/opt/handoff-v*-$(uname -m)-linux/bin/handoff ~/.local/bin/handoff
```

## Comparison with other tools

The things I could think to compare it to were [Matt Pocock's skills](https://github.com/mattpocock/skills), [beads](https://github.com/gastownhall/beads), coding agent memory systems, and spec-driven coding agent workflows. I haven't used any of these, so I don't know how valuable it is for me to comment on them. If you try this and you've used any of those other tools, I'd be interested to hear from you.

I had Claude Opus 5.5 examine a bunch of these tools and write a comparison. I've put it up as [a gist](https://gist.github.com/broughjt/5d0f4d3d61872b26426991de25f705da). I think it was somewhat obviously biased to tell me that my tool was unique and special, but, eh, I figured I'd make it available.

## License

MIT. See [LICENSE](LICENSE).
