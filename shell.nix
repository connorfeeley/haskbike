# SPDX-FileCopyrightText: 2020-2021 Eelco Dolstra and the flake-compat contributors
#
# SPDX-License-Identifier: MIT

(import
  (
    let lock = builtins.fromJSON (builtins.readFile ./flake.lock); in
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    }
  )
  { src = ./.; }
).shellNix
