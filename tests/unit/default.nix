{ pkgs, makeHostenv }:

# Fast/unit-style suites
(import ./restic.nix { inherit pkgs; })
  // (import ./hostenv-assertions.nix { inherit pkgs makeHostenv; })
  // (import ./hostenv-provider-service.nix { inherit pkgs; })
  // (import ./provider-cli.nix { inherit pkgs; })
  // (import ./backups-repo-host.nix { inherit pkgs makeHostenv; })
  // (import ./postgresql.nix { inherit pkgs; })
  // (import ./mysql-backups.nix { inherit pkgs; })
  // (import ./mysql-backups-dependency.nix { inherit pkgs makeHostenv; })
  // (import ./migrate-tags.nix { inherit pkgs makeHostenv; })
  // (import ./php-app-migrate.nix { inherit pkgs makeHostenv; })
