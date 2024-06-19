{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixpkgs-index-advisor.url = "github:connorfeeley/nixpkgs/feat/pg-index-advisor";

    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # flake-parts and friends.
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Haskell package sources.
    # tmp-postgres needs a new release.
    tmp-postgres = { url = "github:jfischoff/tmp-postgres"; flake = false; };
    beam = { url = "github:connorfeeley/beam/feat/insert-only-on-conflict"; flake = false; };
  };
  outputs = inputs@{ self, flake-parts, ... }:
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

      perSystem = { self', inputs', config, pkgs, ... }:
        let inherit (pkgs) lib;
          haskellOptions = {
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
        in
        {
          # Typically, you just want a single project named "default". But
          # multiple projects are also possible, each using different GHC version.
          haskellProjects.default = {
            # If you have a .cabal file in the root, this option is determined
            # automatically. Otherwise, specify all your local packages here.
            projectRoot = ./.;

            settings = {
              haskbike-core = { self, super, ... }: {
                custom = pkg: pkgs.lib.pipe super.haskbike-core [
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
                      extraLibraries = [ pkgs.stdenv.cc.libcxx ];

                      # testToolDepends = pkg.testToolDepends or [ ] ++ [
                      #   pkgs.postgresql
                      #   pkgs.postgresqlTestHook
                      #   pkgs.cabal-install
                      # ];

                      postPatch = o.postPatch or "" + ''
                        cp ${versionFile} Haskbike/Version.hs
                      '';
                    }))
                  (pkgs.haskell.lib.enableLibraryProfiling)
                ];
                check = true;
              };
              haskbike-client = { self, super, ... }: {
                check = false; # Makes network requests.
              };
              haskbike-database = { self, super, ... }: {
                custom = pkg: pkgs.lib.pipe super.haskbike-database [
                  (pkgs.haskell.lib.compose.addTestToolDepends [ pkgs.postgresql ])
                  (pkgs.haskell.lib.compose.overrideCabal (o: {
                    # Flaky tests (under Nix) on darwin.
                    doCheck = if pkgs.stdenv.isDarwin then false else true;
                  }))
                  (pkgs.haskell.lib.enableLibraryProfiling)
                ];
              };
              haskbike-server = { self, super, ... }: {
                custom = pkg: pkgs.lib.pipe super.haskbike-server [
                  # Replace Version.hs with a generated one, since it requires access to the git directory
                  # to determine the version.
                  (pkgs.haskell.lib.compose.overrideCabal (o: {
                    postInstall = o.postInstall or "" + ''
                      mkdir -p $out/share/haskbike/www/static
                      cp -r ${./static-files}/* $out/share/haskbike/www/static/
                    '';
                  }))
                  (pkgs.haskell.lib.enableLibraryProfiling)
                ];
                check = true;
              };
              haskbike-cli = { self, super, ... }: {
                custom = pkg: pkgs.lib.pipe super.haskbike-cli [
                  (pkgs.haskell.lib.compose.addTestToolDepends [ pkgs.postgresql ])
                  (pkgs.haskell.lib.compose.overrideCabal (o: {
                    doCheck = false; # Polling test makes network requests.
                  }))
                  (pkgs.haskell.lib.enableLibraryProfiling)
                  # Add optparse-applicative completions to the derivation output.
                  (self.generateOptparseApplicativeCompletions [ "haskbike" ])
                ];
              };
            };

            # defaults.packages = { }; # Disable default packages

            # Dependency overrides go here. See https://haskell.flake.page/dependency
            packages = {
              # haskbike.source = ./.;
              # aeson.source = "2.1.2.0";
              # zlib.source = pkgs.zlib;
              tmp-postgres.source = inputs.tmp-postgres;
              beam-core.source = "${inputs.beam}/beam-core";
              beam-migration.source = "${inputs.beam}/beam-migration";
              beam-postgres.source = "${inputs.beam}/beam-postgres";
              beam-sqlite.source = "${inputs.beam}/beam-sqlite";
            } // lib.optionalAttrs (lib.versionOlder config.haskellProjects.default.basePackages.ghc.version "9.4") {
              resource-pool.source = "0.4.0.0";

              # 2023-12-20: latest hackage versions.
              servant.source = "0.20.1";
              servant-client-core.source = "0.20";
              servant-docs.source = "0.13";
              servant-server.source = "0.20";
              servant-client.source = "0.20";
            } // lib.optionalAttrs (lib.versionAtLeast config.haskellProjects.default.basePackages.ghc.version "9.5") {
              pqueue.source = "1.5.0.0";
              postgresql-libpg.source = "0.10.0.0";
            };
            settings = {
              postgresql-libpg.jailbreak = true;
              beam-core.jailbreak = true;
              beam-postgres = {
                jailbreak = true;
                check = false; # Postgres tests are flaky on darwin.
              };
              beam-migrate = {
                jailbreak = true;
                broken = false;
              };
              tmp-postgres.check = false;
            };

            # The base package set representing a specific GHC version.
            # By default, this is pkgs.haskellPackages.
            # You may also create your own. See https://haskell.flake.page/package-set
            # basePackages = pkgs.haskellPackages;
            basePackages = pkgs.haskell.packages.ghc98;

            devShell =
              let
                postgres = lib.hiPrio (inputs'.nixpkgs-index-advisor.legacyPackages.postgresql_16.withPackages (ps: with ps; [
                  pg_partman
                  postgis
                  timescaledb
                  hypopg
                  index-advisor
                ]));
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
                    # hasktags
                    cabal-install
                    hlint
                    doctest
                    stylish-haskell
                    tasty-discover
                    weeder
                    retrie
                    # ghcprofview # broken
                    eventlog2html
                    stan
                    ;
                  inherit (self'.packages)
                    haskbike-completions;
                  # Disable ghcid.
                  # ghcid = null;

                  ### Other tools.
                  inherit (pkgs)
                    clang# Faster to link with clang
                    stack
                    reuse
                    litecli
                    pgformatter
                    nixos-rebuild
                    pgtop
                    pg_activity

                    timescaledb-tune
                    timescaledb-parallel-copy

                    ghostscript
                    ;
                  inherit (pkgs.nodePackages) prettier;
                  # PostgreSQL with extensions
                  inherit postgres;
                  # inherit (pkgs.haskellPackages) cabal-plan;

                  treefmt = config.treefmt.build.wrapper;
                } // config.treefmt.build.programs;

                hlsCheck.enable = false;

                # Extra arguments to pass to mkShell.
                mkShellArgs = {
                  # inputsFrom = [ postgres ];
                  # nativeBuildInputs = [ postgres ];
                };
              };
          };

          # haskell-flake doesn't set the default package, but you can do it here.
          packages.default = self'.packages.haskbike-cli;
          packages.haskbike-completions = pkgs.writeTextFile {
            name = "haskbike-completions";
            text = "source <(haskbike --bash-completion-script haskbike)";
            destination = "/etc/bash_completion.d/haskbike";
          };

          packages.haskbike-server-files = pkgs.stdenv.mkDerivation {
            pname = "haskbike-server-files";
            version = rev;
            src = ./static-files;
            installPhase = ''
              mkdir -p $out/share/haskbike/www/static
              cp -r $src/* $out/share/haskbike/www/static/
            '';
          };

          packages.haskbike-cli-static = pkgs.symlinkJoin {
            name = "haskbike-cli-static";
            paths = [
              (pkgs.haskell.lib.justStaticExecutables self'.packages.haskbike-cli)
              self'.packages.haskbike-server-files
            ];
          };

          # Display a graph of all modules and how they depend on each other
          # Run with: nix run .#module-deps-with-filetype -- pdf > /tmp/haskbike.pdf
          packages.module-deps-with-filetype = pkgs.writeShellScriptBin "mainserv-module-deps-with-filetype" ''
            shopt -s globstar
            filetype="$1"
            ${config.haskellProjects.default.basePackages.graphmod}/bin/graphmod \
            ${/*silence warnings for missing external dependencies*/""} \
            --quiet \
            ${/*applies some kind of import simplification*/""} \
            --prune-edges \
            ./haskbike-*/**/*.hs \
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

            settings.formatter.stylish-haskell.excludes = [ "./*/test/Driver.hs" ];
          };
        };
    });
}
