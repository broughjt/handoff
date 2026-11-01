# handoff

## Installation

If you use Nix, installing is really easy. If you don't use Nix, why aren't you using Nix? It's like [XKCD 353](https://xkcd.com/353/) for package managers. Especially now that you mostly don't have to write any Nix by hand anymore, I don't know why you would use any other package manager. But anyway, if you don't use Nix you'll have to install from source.

(TODO: Make a binary release on the Github?)

### With Nix

The flake provides a home-manager module (this requires home-manager 26.05 or later). Add the flake as an input:

TODO: Say why we need 26.05 or later. Should we just say we need 26.11?

```nix
inputs.handoff.url = "github:broughjt/handoff";
# You probably want this to, but it is optional:
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

This puts `handoff` on your `PATH` and adds each skill to `programs.claude-code.skills` and `programs.codex.skills` when those programs are enabled. You can override this with the `programs.handoff.enableClaudeCodeIntegration` and `programs.handoff.enableCodexIntegration` options. For any other agent harness, you can link the skills up yourself by using the `programs.handoff.skills` option, which maps each skill's name to its directory in the package.

(TODO: Add support for the Pi coding agent?)

Without home-manager, there is an overlay, `overlays.default`, that provides `pkgs.handoff`, and the `pkgs.handoff.skills` passthru is a directory that holds the handoff skills.

### Without Nix

There are three components in the installation:

- The `handoff` command
- The `HANDOFF.md` file, which needs to go in an XDG data directory (e.g. `~/.local/share/handoff` or `/usr/local/share/handoff`)
- The three skills, `handoff-create`, `handoff-start`, and `handoff-update`, which need to your agent harness skills directory

For the skills, if you've got Node, there is a `skills` CLI from Vercel you can use:

```sh
npx skills add broughjt/handoff -g -a claude-code -a codex
```

The `-g` means to install to a global user directory instead of installing locally to the project, and the `-a` option specifies which agent harnesses to target. See <https://github.com/vercel-labs/skills/blob/main/docs/commands.md#skills-use>.

If you don't use Node, just manually copy the `handoff-start/`, `handoff-update`, and `handoff-create` directories to `~/.claude/skills` or `~/.agent/skills` or wherever (see <https://code.claude.com/docs/en/claude-directory> or <https://learn.chatgpt.com/docs/build-skills#where-codex-loads-local-skills>).

Next, install Racket 9.3 or later if you don't have it. To build the `handoff` command from source, first install `toml-lib`, `gregor`, `tzdata`, `basedir`, and `typed-racket-lib` as dependencies. 

```sh
git clone https://github.com/broughjt/handoff
cd handoff
raco pkg install --batch --auto --no-docs \
  toml-lib gregor-lib tzdata basedir typed-racket-lib
```

Currently, `toml-lib` has a bug that causes it to misread floats such as `0.05`, so we patch it:

(TODO: Say I've submitted a PR once we have.)

```sh
patch -p1 -d "$(racket -l racket/base -l pkg/lib \
  -e '(display (pkg-directory "toml-lib"))')" < nix/toml-lib-floats.patch
raco setup --pkgs toml-lib
```

Then build the executable:

```sh
raco make handoff/main.rkt
mkdir build
raco exe -o build/handoff handoff/main.rkt
raco distribute dist build/handoff
```

Copy the `dist` directory somewhere permanent. Something like:

```sh
mkdir -p ~/.local/opt
cp -r dist ~/.local/opt/handoff
```

Then add `~/.local/opt/handoff/bin` to `PATH`.

Also copy the data files from this repository:

```sh
mkdir -p ~/.local/share/handoff
cp -r HANDOFF.md workflows ~/.local/share/handoff/
```
