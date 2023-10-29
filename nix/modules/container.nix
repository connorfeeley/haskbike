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
  initialHashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXwfYATGpZ/8EH8+i6idMaSWEW3EfgvT/cXy4zmgGbQRfHlG7jc8qokUtAy1xR4tSk8979bEzHZnBQ5LUGpu4a7W0ufd2uCg0OOxDs7lPPsxmrl4hdkn9kfw0fIdEpUej3EFuQjJcdLYT6z3zqK1KCzosa9AEiEwaflnI5+abqVhQ0q2IchYQqNxfpAIigxQ07h+EA7hJiOl8Vt9/z8Iky+iLnvdT6v1QO2XOhqD2uO+LzBThQ/5wJXsueLUw05FAe5zVCx55K1ui6HvMrgHUZ/rVSQr5X9AYvgCBwUPpY3TuyLBepHG4egccU8eFIY/uw0LFxN1Tkj91LA7mLcveVhNoWo6gIGlx6iJXidHPkZlJcAJ+eq4RNf+3gkSZ46m0p0X4hJgurMr5vTzSR4tDOSkrAgdJL6SSqNcnZZuQNg7JJDxRLrWuFup4UBGFb9/odwXa4rAgMP6dol6UhpIgVFklmbfg4FWD8YaJ1M1lVo6Jid6wVypYwpB+t13k5PdxVzjUJeOTV6jdENRE5+gk6GXoLrxYZp7u0JKmxybYcJ0U6H0azp35BKNYJaobqwtFA+3FL/pnpdRmwLWweqzZV46iO6Vq/T5r4fDxY6nc6d210VbAiFTz4HU743O30w/+3P3csu+E4LAaA8PAvJLNFPLBuMzc67mp00E1irz+Z5w== (none)" ];
in
{
  config = {
    flake = rec {
      nixosModules.vm = { ... }: {
        # Make VM output to the terminal instead of a separate window
        virtualisation.vmVariant.virtualisation.graphics = false;
      };
      # nixosModules.haskbike = moduleWithSystem (
      #   perSystem@{ config, pkgs }: # NOTE: only explicit params will be in perSystem
      #   nixos@{ ... }:
      #   {
      #     boot.isContainer = true;

      #     # Let 'nixos-version --json' know about the Git revision
      #     # of this flake.
      #     system.configurationRevision = pkgs.lib.mkIf (self ? rev) self.rev;

      #     # Set NixOS state version to the latest.
      #     system.stateVersion = "23.05";
      #     nix.settings = {
      #       substituters = "https://cfeeley.cachix.org";
      #       trusted-public-keys = "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU";
      #     };

      #     # Network configuration.
      #     networking.useDHCP = false;
      #     networking.firewall.allowedTCPPorts = [ 80 443 ];

      #     # environment.systemPackages = [ config.packages.haskbike ];

      #     # Enable NGINX as a reverse proxy, with LetsEncrypt.
      #     services.nginx.enable = true;
      #     services.nginx.virtualHosts."bikes.cfeeley.org" = {
      #       addSSL = true;
      #       enableACME = true;
      #       root = "/var/www/bikes.cfeeley.org";
      #     };
      #     security.acme = {
      #       acceptTerms = true;
      #       email = "bikes@cfeeley.org";
      #     };
      #   }
      # );
      nixosConfigurations.haskbike-ec2 = withSystem "aarch64-linux" (ctx@{ config, inputs', system, ... }:
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
            # nixosModules.haskbike

            # This module could be moved into a separate file; otherwise we might
            # as well have used ctx.config.packages directly.
            ({ config, lib, packages, pkgs, modulesPath, ... }: {
              imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
              ec2.efi = true;

              nixpkgs.hostPlatform = system;

              # Let 'nixos-version --json' know about the Git revision
              # of this flake.
              system.configurationRevision = pkgs.lib.mkIf (self ? rev) self.rev;

              # Set NixOS state version to the latest.
              system.stateVersion = "23.05";

              # Use personal binary cache.
              nix.settings = {
                substituters = [ "https://cfeeley.cachix.org" ];
                trusted-public-keys = [ "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU" ];
              };

              # Network configuration.
              networking.firewall.allowedTCPPorts = [ 22 26473 80 443 ];

              # Enable Tailscale
              services.tailscale.enable = true;

              users.mutableUsers = false;
              users.users = {
                root = {
                  hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
                  openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXwfYATGpZ/8EH8+i6idMaSWEW3EfgvT/cXy4zmgGbQRfHlG7jc8qokUtAy1xR4tSk8979bEzHZnBQ5LUGpu4a7W0ufd2uCg0OOxDs7lPPsxmrl4hdkn9kfw0fIdEpUej3EFuQjJcdLYT6z3zqK1KCzosa9AEiEwaflnI5+abqVhQ0q2IchYQqNxfpAIigxQ07h+EA7hJiOl8Vt9/z8Iky+iLnvdT6v1QO2XOhqD2uO+LzBThQ/5wJXsueLUw05FAe5zVCx55K1ui6HvMrgHUZ/rVSQr5X9AYvgCBwUPpY3TuyLBepHG4egccU8eFIY/uw0LFxN1Tkj91LA7mLcveVhNoWo6gIGlx6iJXidHPkZlJcAJ+eq4RNf+3gkSZ46m0p0X4hJgurMr5vTzSR4tDOSkrAgdJL6SSqNcnZZuQNg7JJDxRLrWuFup4UBGFb9/odwXa4rAgMP6dol6UhpIgVFklmbfg4FWD8YaJ1M1lVo6Jid6wVypYwpB+t13k5PdxVzjUJeOTV6jdENRE5+gk6GXoLrxYZp7u0JKmxybYcJ0U6H0azp35BKNYJaobqwtFA+3FL/pnpdRmwLWweqzZV46iO6Vq/T5r4fDxY6nc6d210VbAiFTz4HU743O30w/+3P3csu+E4LAaA8PAvJLNFPLBuMzc67mp00E1irz+Z5w== (none)" ];
                };
                ec2-user = {
                  uid = 1000;
                  isNormalUser = true;
                  inherit initialHashedPassword hashedPassword;
                  openssh.authorizedKeys = { inherit keys; };
                  extraGroups = [
                    "wheel"
                    "ec2-user"
                  ];
                };
                haskbike = {
                  uid = 1001;
                  inherit initialHashedPassword hashedPassword;
                  openssh.authorizedKeys = { inherit keys; };
                  isNormalUser = true;
                  extraGroups = [ "haskbike" "nginx" ];
                };
              };

              services.openssh = {
                enable = true;

                openFirewall = true;
                settings.PasswordAuthentication = false;
                settings.PermitRootLogin = lib.mkDefault "prohibit-password";
              };

              # Add Haskbike to system packages
              environment.systemPackages = [
                packages.haskbike
                pkgs.btop
                pkgs.tmux
              ];

              services.earlyoom = {
                enable = true;
                freeSwapThreshold = 2;
                freeMemThreshold = 2;
              };

              # For Tailscale to work as a bridge to the RDS.
              boot.kernel.sysctl = {
                "net.ipv4.ip_forward" = 1;
                "net.ipv6.conf.all.forwarding" = 1;
              };

              # Enable NGINX as a reverse proxy, with LetsEncrypt.
              services.nginx.enable = true;
              services.nginx.user = "haskbike";
              services.nginx.virtualHosts."bikes.cfeeley.org" = {
                forceSSL = true;
                enableACME = true;
                # root = "/var/www/bikes.cfeeley.org";
                locations."/static/" = {
                  alias = "/var/www/bikes.cfeeley.org/static/";
                };
                locations."/auxiliary/" = {
                  alias = "/var/www/bikes.cfeeley.org/auxiliary/";
                };
                locations."/" = {
                  proxyPass = "http://127.0.0.1:8081";
                  proxyWebsockets = true; # needed if you need to use WebSocket
                  extraConfig =
                    # required when the target is also TLS server with multiple hosts
                    "proxy_ssl_server_name on;" +
                    # required when the server wants to use HTTP Authentication
                    "proxy_pass_header Authorization;"
                  ;
                };
              };
              # Configure LetsEncrypt
              security.acme = {
                acceptTerms = true;
                defaults.email = "bikes@cfeeley.org";
              };
            })
          ];
        });
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

    perSystem = { self', config, pkgs, ... }: {
      packages.haskbike-docker = pkgs.dockerTools.buildImage {
        # Name of the container
        name = "haskbike-docker";

        # Install nginx
        copyToRoot = [ self'.packages.haskbike-static pkgs.cacert ];

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
          Cmd = [ "haskbike" "-v" "--plain" "--enable-migrations" "poll" ];
        };
      };
    };
  };
}
