# handoff

TODO: Under construction.

## Installation

If you use Nix, installing is really easy. If you don't use Nix, why aren't you using Nix? It's like [XKCD 353](https://xkcd.com/353/) for package managers. Especially now that you mostly don't have to write any Nix by hand anymore, I don't know why you would use any other package manager. But anyway, if you don't use Nix, you can either install using the Racket package manager `raco` or from the binary release on GitHub.

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

This puts `handoff` on your `PATH`, adds each skill to `programs.claude-code.skills` and `programs.codex.skills` when those programs are enabled, and links each skill into the `skills` directory of `programs.pi-coding-agent.configDir` when Pi is enabled. You can override this with the `programs.handoff.enableClaudeCodeIntegration`, `programs.handoff.enableCodexIntegration` and `programs.handoff.enablePiIntegration` options. For any other agent harness, you can link the skills up yourself by using the `programs.handoff.skills` option, which maps each skill's name to its directory in the package.

Without home-manager, there is an overlay, `overlays.default`, that provides `pkgs.handoff`, and the `pkgs.handoff.skills` passthru is a directory that holds the handoff skills.

### Without Nix

There are two components in the installation:

- The `handoff` command
- The three skills, `handoff-create`, `handoff-start`, and `handoff-update`, which need to go in your agent harness skills directory

For the skills, if you've got Node, there is a `skills` CLI from Vercel you can use:

```sh
npx skills add broughjt/handoff -g -a claude-code -a codex
```

The `-g` means to install to a global user directory instead of installing locally to the project, and the `-a` option specifies which agent harnesses to target. See <https://github.com/vercel-labs/skills/blob/main/docs/commands.md#skills-use>.

If you don't use Node, just manually copy the `handoff-start`, `handoff-update`, and `handoff-create` directories to `~/.claude/skills` or `~/.agents/skills` or wherever (see <https://code.claude.com/docs/en/claude-directory> or <https://learn.chatgpt.com/docs/build-skills#where-codex-loads-local-skills>).

For the `handoff` command installation, you have two options. You can either install as a Racket package or install from a binary release if you're on Linux.

For the first option, install Racket 9.3 or later if you don't have it, and install `handoff` using `raco`:

```sh
raco pkg install --auto git+https://github.com/broughjt/handoff
```

This installs its dependencies and a `handoff` launcher in Racket's user `bin` directory, which is not on `PATH` by default. The following command prints the directory you need to add to `PATH` (e.g. `~/.local/share/racket/9.3/bin`):

```sh
racket -l racket/base -l setup/dirs -e '(displayln (find-user-console-bin-dir))'
```

Currently, `toml-lib` has a bug that causes it to misread floats such as `0.05`, so the package depends on a fork with the fix until the [upstream PR](https://github.com/toml-racket/toml-racket/pull/24) is merged. If you already had `toml-lib` installed, `raco` keeps that copy, and so you need to switch it to the fork:

```sh
raco pkg update 'git+https://github.com/broughjt/toml-racket?path=toml-lib#fix-floats'
```

For the second option, download the tarball from the [latest release](https://github.com/broughjt/handoff/releases/latest), unpack it somewhere permanent, and link the command onto your `PATH`:

```sh
mkdir -p ~/.local/opt ~/.local/bin
tar -xzf handoff-v*-$(uname -m)-linux.tar.gz -C ~/.local/opt
ln -s ~/.local/opt/handoff-v*-$(uname -m)-linux/bin/handoff ~/.local/bin/handoff
```

The tarball also contains the skills in `share/skills/handoff`. The `handoff` command requires `git` and `bash` to be on your `PATH`.
