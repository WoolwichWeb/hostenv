{ pkgs, envs, makeHostenv }:

# Integration / full-stack suites
(import ./drupal/tests.nix { inherit pkgs envs; })
  // (import ./drupal7/tests.nix { inherit pkgs envs; })
  // (import ./provider-plan.nix { inherit pkgs makeHostenv; })
  // (import ./provider-full.nix { inherit pkgs makeHostenv; })
  // (import ./socket-contract.nix { inherit pkgs envs; })
  // { hostenv-hostname = import ./hostname.nix { inherit pkgs makeHostenv; }; }
  // (import ./plan-bridge.nix { inherit pkgs makeHostenv; })
