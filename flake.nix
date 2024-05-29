{
  nixConfig.allow-import-from-derivation = true;
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = inputs:
    inputs.parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];

      perSystem = {
        config,
        pkgs,
        ...
      }: {
        pre-commit = {
          check.enable = true;
          settings.hooks = {
            cabal-fmt.enable = true;
            hlint.enable = true;

            alejandra.enable = true;
            deadnix.enable = true;
          };
        };

        haskellProjects.ghc92 = {
          packages = {};
          settings = {};
          basePackages = pkgs.haskell.packages.ghc92;
          devShell.mkShellArgs.shellHook = config.pre-commit.installationScript;
        };

        haskellProjects.ghc94 = {
          packages = {};
          settings = {};
          basePackages = pkgs.haskell.packages.ghc94;
          devShell.mkShellArgs.shellHook = config.pre-commit.installationScript;
        };
        haskellProjects.default = {
          packages = {};
          settings = {};
          basePackages = pkgs.haskellPackages;
          devShell.mkShellArgs.shellHook = config.pre-commit.installationScript;
        };
      };
    };
}
