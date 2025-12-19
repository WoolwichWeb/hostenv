{ pkgs, envs, makeHostenv }:

# Integration / full-stack suites
(import ./drupal/tests.nix { inherit pkgs envs; })
  // (import ./drupal7/tests.nix { inherit pkgs envs; })
  // { hostenv-cli-list = import ./cli-list.nix { inherit pkgs makeHostenv; }; }
  // (import ./provider-plan.nix { inherit pkgs makeHostenv; })
  // (import ./provider-full.nix { inherit pkgs makeHostenv; })
  // (import ./socket-contract.nix { inherit pkgs envs; })
  // { hostenv-hostname = import ./hostname.nix { inherit pkgs makeHostenv; }; }
  // (import ./plan-bridge.nix { inherit pkgs makeHostenv; })
