{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";

    rust-overlay.url = "github:oxalica/rust-overlay";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    pre-commit = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.rust-overlay.overlays.default
          ];
        };
        haskellProjects.default = {
          settings = { };
          # overrides = self: super: { };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
          devShell = {
            hlsCheck.enable = false;
          };
        };
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };
        pre-commit.settings.hooks.treefmt.enable = true;
        # # Dev shell scripts.
        # mission-control.scripts = {
        #   docs = {
        #     description = "Start Hoogle server for project dependencies";
        #     exec = ''
        #       echo http://127.0.0.1:8888
        #       hoogle serve -p 8888 --local
        #     '';
        #     category = "Dev Tools";
        #   };
        #   repl = {
        #     description = "Start the cabal repl";
        #     exec = ''
        #       cabal repl "$@"
        #     '';
        #     category = "Dev Tools";
        #   };
        #   fmt = {
        #     description = "Format the source tree";
        #     exec = "${lib.getExe config.treefmt.build.wrapper}";
        #     category = "Dev Tools ";
        #   };
        #   run = {
        #     description = "Run the project with ghcid auto-recompile";
        #     exec = ''
        #       ghcid -c "cabal repl exe:haskell-template" --warnings -T :main
        #     '';
        #     category = "Primary";
        #   };
        # };

        # Default shell.
        devShells.default = pkgs.mkShell {
          inputsFrom = [

            config.pre-commit.devShell
            config.treefmt.build.devShell
            config.haskellProjects.default.outputs.devShell
          ];
          buildInputs = [
          ];
        };
        packages.default = config.packages.aoc;
      };
    };
}
