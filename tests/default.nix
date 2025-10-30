{ pkgs, envs }:

# Merge suite-specific checks (each returns an attrset of derivations)
(import ./drupal/tests.nix { inherit pkgs envs; })
  // (import ./drupal7/tests.nix { inherit pkgs envs; })
