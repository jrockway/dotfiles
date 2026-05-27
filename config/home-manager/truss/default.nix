{
  pkgs,
  lib,
  pyproject-nix,
  uv2nix,
  pyproject-build-systems,
  truss-src,
}:
let
  workspace = uv2nix.lib.workspace.loadWorkspace { workspaceRoot = truss-src; };
  # Prefer prebuilt wheels — avoids pulling Rust/C toolchains for
  # truss-transfer, blake3, watchfiles, etc.
  overlay = workspace.mkPyprojectOverlay { sourcePreference = "wheel"; };
  python = pkgs.python313;
  pythonSet =
    (pkgs.callPackage pyproject-nix.build.packages {
      inherit python;
    }).overrideScope
      (
        lib.composeManyExtensions [
          pyproject-build-systems.overlays.default
          overlay
        ]
      );
  # Only the truss package's runtime deps — excludes the dev / dev-server
  # groups that pyproject.toml's `default-groups` lists.
  venv = pythonSet.mkVirtualEnv "truss-env" { truss = [ ]; };
in
# Expose only the truss console scripts. The venv contains its own Python
# and assorted scripts (httpx, keyring, ...) that would collide with other
# packages in the user's profile.
pkgs.runCommand "truss-${venv.version or "0"}"
  {
    meta.mainProgram = "truss";
  }
  ''
    mkdir -p $out/bin
    ln -s ${venv}/bin/truss $out/bin/truss
    ln -s ${venv}/bin/truss-docker-build-setup $out/bin/truss-docker-build-setup
  ''
