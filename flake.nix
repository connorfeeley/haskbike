{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # If you have a .cabal file in the root, this option is determined
          # automatically. Otherwise, specify all your local packages here.
          projectRoot = ./.;

          # defaults.packages = { }; # Disable default packages
          # packages = {
          #   haskbike.source = ./.;
          #   aeson.source = "2.1.2.0";
          # };

          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;
          basePackages = pkgs.haskell.packages.ghc810;

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          # source-overrides = { };
          # packages = {
          #   zlib.source = pkgs.zlib;
          # };

          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              ### Haskell tools.
              inherit (hp)
                haskell-language-server
                implicit-hie
                floskell
                hasktags
                cabal-install
                hlint
                # stylish-haskell
                ;
              # Disable ghcid.
              # ghcid = null;

              ### Other tools.
              inherit (pkgs)
                stack
                reuse
                postgresql
                pgadmin
                litecli
              ;
            };

            hlsCheck.enable = false;
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.haskbike;
      };
    };
}
