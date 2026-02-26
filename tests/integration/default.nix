{ pkgs, envs, makeHostenv, inputs }:

# Integration / full-stack suites
(import ./drupal/tests.nix { inherit pkgs envs; })
  // (import ./drupal7/tests.nix { inherit pkgs envs; })
  // { hostenv-cli-list = import ./cli-list.nix { inherit pkgs makeHostenv; }; }
  // { hostenv-cli-secrets-sync = import ./cli-secrets-sync.nix { inherit pkgs makeHostenv; }; }
  // { hostenv-outputs-eval = import ./hostenv-outputs.nix { inherit pkgs makeHostenv inputs; }; }
  // (import ./provider-plan.nix { inherit pkgs makeHostenv inputs; })
  // (import ./provider-full.nix { inherit pkgs makeHostenv inputs; })
  // (import ./provider-nixos-system.nix { inherit pkgs inputs; })
  // { local-provider-migration-test = import ./local-provider-migration.nix { inherit pkgs; }; }
  // { provider-service-module-eval = import ./provider-service-module.nix { inherit pkgs makeHostenv; }; }
  // { devshells-eval = import ./devshells.nix { inherit pkgs makeHostenv inputs; }; }
  // (import ./socket-contract.nix { inherit pkgs envs; })
  // { hostenv-hostname = import ./hostname.nix { inherit pkgs makeHostenv; }; }
  // { public-env-json = import ./public-env-json.nix { inherit pkgs makeHostenv; }; }
