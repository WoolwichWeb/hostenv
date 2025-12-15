{ pkgs, makeHostenv }:

# Fast/unit-style suites
(import ./restic.nix { inherit pkgs; })
  // (import ./users-slices.nix { inherit pkgs; })
  // (import ./hostenv-assertions.nix { inherit pkgs makeHostenv; })
