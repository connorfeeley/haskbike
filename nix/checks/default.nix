# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause


{ pkgs }:
{
  no-op = pkgs.runCommand
    "no-op"
    {
      passthru.foo = "foo";
    }
    ''
      set -euo pipefail

      true

      touch $out
    '';
}
