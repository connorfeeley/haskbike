# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

# Create .nixd.json file with:
#   nix eval --json --file .nixd.nix > .nixd.json

{
  # eval = {
  #   # Example target for writing a package.
  #   target = {
  #     # args = [ "-f" "default.nix" ];
  #     args = [ "--expr" "with import <nixpkgs> { }; callPackage ./default.nix { }" ];
  #     installable = "packages.aarch64-darwin.default";
  #   };
  #   # Force thunks
  #   depth = 10;
  # };
  formatting.command = "nixpkgs-fmt";
  options = {
    enable = true;
    target = {
      args = [ ];
      # Example installable for flake-parts, nixos, and home-manager

      # flake-parts
      installable = ".#debug.options";

      # nixOS configuration
      # installable = "/flakeref#nixosConfigurations.<adrastea>.options";

      # home-manager configuration
      # installable = "/flakeref#homeConfigurations.<name>.options";
    };
  };
}
