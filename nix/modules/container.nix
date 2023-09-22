# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, lib, inputs, flake-parts-lib, moduleWithSystem, withSystem, ... }:

let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    mkOption
    mkPackageOption
    types;
in
{
  config = {
    flake = rec {
      nixosModules.container = moduleWithSystem (
        perSystem@{ config, pkgs }: # NOTE: only explicit params will be in perSystem
        nixos@{ ... }:
        {
          boot.isContainer = true;

          # Let 'nixos-version --json' know about the Git revision
          # of this flake.
          system.configurationRevision = pkgs.lib.mkIf (self ? rev) self.rev;

          # Network configuration.
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 ];

          # Enable a web server.
          services.httpd = {
            enable = true;
            adminAddr = "morty@example.org";
          };
        }
      );
      nixosConfigurations.container = withSystem "x86_64-linux" (ctx@{ config, inputs', system, ... }:
        inputs.nixpkgs.lib.nixosSystem {
          # Expose `packages`, `inputs` and `inputs'` as module arguments.
          # Use specialArgs permits use in `imports`.
          # Note: if you publish modules for reuse, do not rely on specialArgs, but
          # on the flake scope instead. See also https://flake.parts/define-module-in-separate-file.html
          specialArgs = {
            packages = config.packages;
            inherit inputs inputs';
          };
          modules = [
            nixosModules.container
            # This module could be moved into a separate file; otherwise we might
            # as well have used ctx.config.packages directly.
            ({ config, lib, packages, pkgs, ... }: {
              imports = [ ];
              nixpkgs.hostPlatform = system;
            })
          ];
        });
    };
  };
}
