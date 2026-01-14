{ pkgs, makeHostenv }:

# Fast/unit-style suites
(import ./restic.nix { inherit pkgs; })
  // (import ./hostenv-assertions.nix { inherit pkgs makeHostenv; })
  // (import ./hostenv-provider-service.nix { inherit pkgs; })
  // (import ./backups-repo-host.nix { inherit pkgs makeHostenv; })
