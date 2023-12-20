{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # flake-parts and friends.
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    let rev = self.rev or self.dirtyRev or "dirty";
    in
    flake-parts.lib.mkFlake { inherit inputs; } ({ ... }: {
      debug = true;
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule

        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];

      perSystem = { self', config, pkgs, ... }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # If you have a .cabal file in the root, this option is determined
          # automatically. Otherwise, specify all your local packages here.
          projectRoot = ./.;

          settings = {
            haskbike = { self, super, ... }: {
              custom = _pkg: pkgs.lib.pipe super.haskbike [
                # Replace Version.hs with a generated one, since it requires access to the git directory
                # to determine the version.
                (pkgs.haskell.lib.compose.overrideCabal (o:
                  let
                    versionFile = pkgs.substituteAll {
                      src = ./nix/VersionPure.hs;
                      inherit rev;
                    };
                  in
                  {
                    postPatch = o.postPatch or "" + "cp ${versionFile} src-lib/Version.hs";
                    doCheck = false;
                    postInstall = o.postInstall or "" + ''
                      mkdir -p $out/share/haskbike/www/static
                      cp -r ${./static-files}/* $out/share/haskbike/www/static/
                    '';
                  }))

                # Add optparse-applicative completions to the derivation output.
                (self.generateOptparseApplicativeCompletions [ "haskbike" ])
              ];

              extraBuildTools = [ pkgs.git ];

              check = false; # Don't run cabal tests as part of build.

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

          # defaults.packages = { }; # Disable default packages

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          packages = {
            # haskbike.source = ./.;
            # aeson.source = "2.1.2.0";
            # zlib.source = pkgs.zlib;

            resource-pool.source = "0.4.0.0";

            # 2023-12-20: latest hackage versions.
            servant.source = "0.20.1";
            servant-client-core.source = "0.20";
            servant-docs.source = "0.13";
            servant-server.source = "0.20";
            servant-client.source = "0.20";
          };

          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;
          basePackages = pkgs.haskell.packages.ghc928;

          devShell =
            let
              postgres = pkgs.postgresql_15.withPackages (ps: with ps; [ pg_partman postgis timescaledb ]);
            in
            {
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
                  tasty-discover
                  weeder
                  retrie
                  ;
                inherit (self'.packages)
                  haskbike-completions;
                # Disable ghcid.
                # ghcid = null;

                ### Other tools.
                inherit (pkgs)
                  stack
                  reuse
                  pgadmin
                  litecli
                  pgformatter
                  nixos-rebuild
                  pgtop
                  pg_activity

                  timescaledb-tune
                  timescaledb-parallel-copy
                  ;
                inherit (pkgs.llvmPackages) bintools;
                inherit (pkgs.nodePackages) prettier;
                # PostgreSQL with extensions
                inherit postgres;

                treefmt = config.treefmt.build.wrapper;
              } // config.treefmt.build.programs;

              hlsCheck.enable = false;
            };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.haskbike;
        packages.haskbike-completions = pkgs.writeTextFile {
          name = "haskbike-completions";
          text = "source <(haskbike --bash-completion-script haskbike)";
          destination = "/etc/bash_completion.d/haskbike";
        };

        packages.haskbike-static = pkgs.haskell.lib.justStaticExecutables self'.packages.haskbike;

        # Display a graph of all modules and how they depend on each other
        packages.module-deps-with-filetype = pkgs.writeShellScript "mainserv-module-deps-with-filetype" ''
          shopt -s globstar
          filetype="$1"
          ${config.haskellProjects.default.basePackages.graphmod}/bin/graphmod \
          ${/*silence warnings for missing external dependencies*/""} \
          --quiet \
          ${/*applies some kind of import simplification*/""} \
          --prune-edges \
          ./src-*/**/*.hs \
          | ${pkgs.graphviz}/bin/dot \
            ${/*otherwise it’s a bit cramped*/""} \
            -Gsize="20,20!" \
            -T"$filetype"
        '';

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          # This is the default, and can be overriden.
          package = pkgs.treefmt;
          # formats .hs files
          programs.stylish-haskell.enable = true;
          # formats .nix files
          programs.nixpkgs-fmt.enable = true;
          # formats .cabal files
          programs.cabal-fmt.enable = false;
          # Suggests improvements for your code in .hs files
          programs.hlint.enable = false;

          settings.formatter.stylish-haskell.excludes = [ "./test/Driver.hs" ];
        };
      };
    });
}
