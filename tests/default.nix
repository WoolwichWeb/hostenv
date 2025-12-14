{ pkgs, envs, makeHostenv }:

# Merge suite-specific checks (each returns an attrset of derivations)
(import ./drupal/tests.nix { inherit pkgs envs; })
  // (import ./drupal7/tests.nix { inherit pkgs envs; })
  // (import ./provider-plan.nix { inherit pkgs makeHostenv; })
  // (import ./socket-contract.nix { inherit pkgs envs; })
  // { hostenv-hostname = import ./hostname.nix { inherit pkgs makeHostenv; }; }
  // (import ./plan-bridge.nix { inherit pkgs makeHostenv; })
  // (import ./users-slices.nix { inherit pkgs; })
  // (import ./restic.nix { inherit pkgs; })
