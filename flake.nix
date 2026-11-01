{
  description = "Coding agent skills for context management across many sessions";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        racketDependencies = pkgs.callPackage ./nix/racket-dependencies.nix { };
        handoff = pkgs.callPackage ./nix/package.nix { inherit racketDependencies; };
      in
      {
        packages = {
          default = handoff;
          inherit handoff;
          racket-dependencies = racketDependencies;
        };

        # The build runs the test suite.
        checks.default = handoff;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            racket
            bash
            git
          ];
          PLTADDONDIR = racketDependencies;
        };
      }
    )
    // {
      overlays.default = final: _prev: {
        handoff = final.callPackage ./nix/package.nix {
          racketDependencies = final.callPackage ./nix/racket-dependencies.nix { };
        };
      };

      homeModules.default = import ./nix/home-manager.nix { inherit self; };

      # Superseded by the package's passthru.skills and the home-manager module.
      skills = ./skills;
    };
}
