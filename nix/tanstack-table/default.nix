# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib
, fetchFromGitHub
, fetchPnpmDeps
, pnpmConfigHook
, stdenv
, nodePackages
, git
, fetchgit
}:

stdenv.mkDerivation (finalAttrs: rec {
  pname = "table";
  version = "8.13.2";

  src = fetchgit {
    url = "https://github.com/TanStack/table.git";
    rev = "v${version}";
    hash = "sha256-v8ogS7marneQ9rHyAj+g1CdigEyXUEozHoX9q+crlOE=";
    leaveDotGit = true;
  };

  nativeBuildInputs = [
    pnpmConfigHook
    nodePackages.pnpm
    git
  ];

  postPatch = ''
    substituteInPlace package.json \
      --replace-fail \
      "nx affected --targets=build --exclude=examples/** && size-limit" \
      "nx affected --base=HEAD --targets=build --exclude=examples/**"
  '';

  postBuild = ''
    for PACKAGE in packages/*/; do
      pnpm exec nx run @tanstack/$(basename $PACKAGE):build
    done
  '';

  installPhase = ''
    mkdir -p $out

    for PACKAGE in packages/*/; do
      mkdir -p "$out/$(basename $PACKAGE)"
      ls -al "''${PACKAGE}"
      ls -al "''${PACKAGE}/build"
      cp -r ''${PACKAGE}/build/* "$out/$(basename "$PACKAGE")"
    done
    '';

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) src pname;
    hash = "sha256-3oOM/wLvew39OyOvXg2ejYRX4DulKGCPWA6/Nr6FBfg=";
  };

  meta = with lib; {
    description = "Framework-agnostic headless table UI library";
    homepage = "https://tanstack.com/table";
    license = licenses.mit;
    maintainers = with maintainers; [ cfeeley ];
  };
})
