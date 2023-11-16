{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, moduleWithSystem, flake-parts-lib, ... }:
      let
        inherit (flake-parts-lib) importApply;
        flakeModules.default = flakeModules.haskbike;
        flakeModules.haskbike = importApply ./flake-module.nix { inherit withSystem; };
      in
      {
        debug = true;
        systems = nixpkgs.lib.systems.flakeExposed;
        imports = [
          inputs.haskell-flake.flakeModule

          inputs.treefmt-nix.flakeModule
          inputs.flake-root.flakeModule

          flakeModules.haskbike
        ];

        # Export flakeModules.
        flake = { inherit flakeModules; };
      });
}
