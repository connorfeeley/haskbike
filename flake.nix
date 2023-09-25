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
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule

        # Import this repo's modules.
        ./nix/modules
      ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # If you have a .cabal file in the root, this option is determined
          # automatically. Otherwise, specify all your local packages here.
          projectRoot = ./.;

          settings = {
            haskbike = {
              check                = false;   # Don't run cabal tests as part of build.

              # Profiling options.
              # libraryProfiling     = false;   # Disable profiling for libraries.
              # executableProfiling  = false;   # Disable profiling for executables.

              # Debugging options.
              # strip                = false;   # Don't strip executables.
              # enableDWARFDebugging = true;    # Enable DWARF debugging.
              # deadCodeElimination  = false;   # Disable dead code elimination.

              # disableOptimization  = true;    # Disable optimization - significantly speeds up build time.

              # Documentation options.
              # haddock              = true;    # Enable haddock generation.
              # hyperlinkSource      = true;    # Enable hyperlinked source.
            };
          };

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          # source-overrides = { };

          # defaults.packages = { }; # Disable default packages
          # packages = {
          #   haskbike.source = ./.;
          #   aeson.source = "2.1.2.0";
          #   zlib.source = pkgs.zlib;
          # };

          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;
          basePackages = pkgs.haskell.packages.ghc928;

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
                doctest
                stylish-haskell
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

        packages.haskbike-static = pkgs.haskell.lib.justStaticExecutables self'.packages.haskbike;

        packages.haskbike-docker = pkgs.dockerTools.buildImage {
          # Name of the container
          name = "haskbike-docker";

          # Install nginx
          contents = [ self'.packages.haskbike-static pkgs.cacert ];

          # Extra build commands
          # extraCommands = ''
          # '';

          # Create the user
          # runAsRoot = ''
          #   #!${pkgs.stdenv.shell}
          #   ${pkgs.dockerTools.shadowSetup}
          #   groupadd --system nginx
          #   useradd --system --gid nginx nginx
          # '';

          # Start the service and expose the port
          config = {
            Cmd = [ "haskbike" ];
          };
        };
      };
    };
}
