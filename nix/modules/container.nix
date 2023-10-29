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
      nixosModules.vm = { ... }: {
        # Make VM output to the terminal instead of a separate window
        virtualisation.vmVariant.virtualisation.graphics = false;
      };
      nixosModules.haskbike = moduleWithSystem (
        perSystem@{ config, pkgs }: # NOTE: only explicit params will be in perSystem
        nixos@{ ... }:
        {
          boot.isContainer = true;

          # Let 'nixos-version --json' know about the Git revision
          # of this flake.
          system.configurationRevision = pkgs.lib.mkIf (self ? rev) self.rev;

          # Set NixOS state version to the latest.
          system.stateVersion = "23.05";

          # Network configuration.
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 443 ];

          environment.systemPackages = [ config.packages.haskbike ];

          # Enable NGINX as a reverse proxy, with LetsEncrypt.
          services.nginx.enable = true;
          services.nginx.virtualHosts."bikes.cfeeley.org" = {
            addSSL = true;
            enableACME = true;
            root = "/var/www/bikes.cfeeley.org";
          };
          security.acme = {
            acceptTerms = true;
            email = "bikes@cfeeley.org";
          };
        }
      );
      nixosConfigurations.container = withSystem "aarch64-linux" (ctx@{ config, inputs', system, ... }:
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
            # Import the module defined above.
            nixosModules.haskbike

            # This module could be moved into a separate file; otherwise we might
            # as well have used ctx.config.packages directly.
            ({ config, lib, packages, pkgs, ... }: {
              imports = [ ];
              nixpkgs.hostPlatform = system;
            })
          ];
        });
      nixosConfigurations.vm = withSystem "aarch64-linux" (ctx@{ config, inputs', system, ... }:
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
            # Import the module defined above.
            nixosModules.vm

            # This module could be moved into a separate file; otherwise we might
            # as well have used ctx.config.packages directly.
            ({ config, lib, packages, pkgs, ... }: {
              imports = [ ];
              nixpkgs.hostPlatform = system;
            })
          ];
        });
      nixosConfigurations.darwin-vm = withSystem "aarch64-linux" (ctx@{ config, inputs', system, ... }:
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
            # Import the module defined above.
            nixosModules.vm

            # This module could be moved into a separate file; otherwise we might
            # as well have used ctx.config.packages directly.
            ({ config, lib, packages, pkgs, inputs, ... }: {
              imports = [ ];
              nixpkgs.hostPlatform = system;
              virtualisation.vmVariant.virtualisation.host.pkgs = inputs.nixpkgs.legacyPackages.aarch64-darwin;
            })
          ];
        });
      packages.aarch64-darwin.darwin-vm = self.nixosConfigurations.darwin-vm.config.system.build.vm;
    };
  };
}
