{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ./support { inherit pkgs lib; };
  providerView = support.providerView;
  asserts = support.asserts;

  envFixtures = import ./environments.nix { inherit pkgs makeHostenv; };
  drupalEnvs = envFixtures.drupalProduction.config.environments;

  providerEnvs =
    let
      withExtras = drupalEnvs // {
        main = drupalEnvs.main // {
          hostenv = (drupalEnvs.main.hostenv or { }) // {
            extras = {
              backups = {
                repo = "s3:https://backups.example/test";
                passwordFile = "/run/secrets/main/backups_secret";
                envFile = "/run/secrets/main/backups_env";
                dataDir = "/var/lib/test-data";
              };
            };
          };
        };
        dev = drupalEnvs.dev // { enable = false; };
      };
    in providerView withExtras;

  planBridgeEval = lib.evalModules {
    specialArgs = { inherit pkgs; };
    modules = [
      support.stubs.base
      ../modules/core/environments.nix
      ../modules/nixos/plan-bridge.nix
      ../modules/nixos/nginx-hostenv.nix
      ../modules/nixos/backups-hostenv.nix
      ({ ... }: {
        hostenv = {
          organisation = "test";
          project = "test-project";
          hostenvHostname = "hosting.test";
          environmentName = "meta";
          root = ".";
        };
        environments = providerEnvs;
        defaultEnvironment = "main";
        hostenv.backups.enable = true;
        hostenv.backupsRepoHost = "s3:https://backups.example";
      })
    ];
  };

  upstreams = planBridgeEval.config.services.nginx.upstreams or { };
  restic = planBridgeEval.config.services.restic.backups or { };

  upstreamsTest = asserts.assertTrue "dendritic-nginx-disabled-envs"
    (builtins.length (lib.attrNames upstreams) == 2)
    "expected 2 upstreams (enabled envs), got ${toString (builtins.length (lib.attrNames upstreams))}";

  backupsTest = asserts.assertTrue "dendritic-backups-enabled-only"
    (restic ? main && (builtins.length (lib.attrNames restic) == 1)
      && lib.elem "/var/lib/test-data" (restic.main.paths or [ ]))
    "restic backups should contain only main with test-data path";

in {
  disabled_envs_filtered = upstreamsTest;
  backups_filtered = backupsTest;
}
