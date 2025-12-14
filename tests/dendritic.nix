{ pkgs, makeHostenv }:

let
  lib = pkgs.lib;
  support = import ./support { inherit pkgs lib; };
  stubs = support.stubs;
  providerView = support.providerView;
  samples = support.samples;

  # Reuse the real Drupal fixtures so host-level tests stay aligned with the
  # client/runtime layer.
  envFixtures = import ./environments.nix { inherit pkgs makeHostenv; };
  drupalEnvs = envFixtures.drupalProduction.config.environments;

  # Adapt fixture data into the provider-style shape consumed by plan-bridge.
  # - Disable the dev env to exercise filtering.
  # - Add backups extras to main only so backups-hostenv can discriminate.
  providerEnvs =
    let
      withExtras =
        drupalEnvs // {
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
    in
    providerView withExtras;

  planBridgeEval = lib.evalModules {
    specialArgs = { inherit pkgs; };
    modules = [
      stubs.base
      ../modules/core/environments.nix
      ../modules/nixos/plan-bridge.nix
      ../modules/nixos/nginx-hostenv.nix
      ../modules/nixos/backups-hostenv.nix
      ({ ... }: {
        # _module.check = false;
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

  upstreamsTest = support.asserts.jqAssert
    "dendritic-nginx-disabled-envs"
    "length == 2"
    (planBridgeEval.config.services.nginx.upstreams or { });

  backupsTest = support.asserts.jqAssert
    "dendritic-backups-enabled-only"
    ''has("main") and (length==1) and (.main.paths | any(.=="/var/lib/test-data"))''
    (planBridgeEval.config.services.restic.backups or { });

  # Keep the slice regression guard (stubbed options are sufficient).
  sliceEval =
    let
      envs = samples.mkSingle { name = "alpha"; userName = "alpha"; publicKeys = [ ]; };
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          stubs.base
          ../modules/core/environments.nix
          ../modules/nixos/plan-bridge.nix
          ../modules/nixos/users-slices.nix
          ({ ... }: {
            _module.check = false;
            environments = envs;
            defaultEnvironment = "alpha";
          })
        ];
      };
      slice = lib.head (lib.attrValues eval.config.systemd.slices);
    in
    support.asserts.jqAssert "users-slices-configured"
      ''.sliceConfig.CPUAccounting=="yes" and (.sliceConfig.MemoryMax|test("^[0-9]+G$"))''
      slice;

in
{
  slice_defaults_applied = sliceEval;
  disabled_envs_filtered = upstreamsTest;
  backups_filtered = backupsTest;
  slice_respects_custom_user =
    let
      envs = samples.mkSingle {
        name = "envWithCustomUser";
        userName = "customuser";
        publicKeys = [ ];
      };
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          stubs.base
          ../modules/core/environments.nix
          ../modules/nixos/plan-bridge.nix
          ../modules/nixos/users-slices.nix
          ({ ... }: {
            _module.check = false;
            environments = envs;
            defaultEnvironment = "envWithCustomUser";
          })
        ];
      };
      slices = eval.config.systemd.slices;
      users = eval.config.users.users;
    in
    support.asserts.jqAssert "users-slices-custom-user"
      ''(.slices|keys|length)==1 and (.users|keys|length)==1 and ((.slices|keys)[0]|sub("\\.slice$";"")) == (.users|keys)[0]''
      { inherit slices users; };
  restic_exclusive_repo =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          stubs.base
          ../modules/env/restic.nix
          ({ ... }: {
            hostenv.cacheDir = "/tmp/hostenv-cache";
            hostenv.userName = "restic-test";
            hostenv.backupsRepoHost = "s3:https://backups.invalid";
          })
          ({ ... }: {
            _module.check = false;
            services.restic.enable = true;
            services.restic.backups.bad = {
              paths = [ "/data" ];
              repository = "s3:https://example.invalid";
              repositoryFile = pkgs.writeText "repo-file" "s3:https://other.invalid";
              environmentFile = null;
              passwordFile = "/tmp/pw";
            };
          })
        ];
      };
      assertionsJson = builtins.toFile "assertions.json" (builtins.toJSON eval.config.assertions);
    in
    pkgs.runCommand "restic-exclusive-repo-assert" { buildInputs = [ pkgs.jq pkgs.coreutils ]; } ''
      cat ${assertionsJson} > $out
      if jq -e 'map(.assertion == false) | any' "$out" >/dev/null; then
        exit 0
      fi
      echo "Expected restic assertion to fail when repository and repositoryFile are both set" >&2
      cat "$out" >&2
      exit 1
    '';

  restic_repo_envfile_ok =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          stubs.base
          ../modules/env/restic.nix
          ({ ... }: {
            hostenv.cacheDir = "/tmp/hostenv-cache";
            hostenv.userName = "restic-test";
            hostenv.backupsRepoHost = "s3:https://backups.invalid";
          })
          ({ ... }: {
            _module.check = false;
            services.restic.enable = true;
            services.restic.backups.ok = {
              paths = [ "/data" ];
              repository = "s3:https://example.invalid";
              repositoryFile = null;
              environmentFile = "/tmp/envfile";
              passwordFile = "/tmp/pw";
            };
          })
        ];
      };
      assertionsJson = pkgs.writeText "assertions.json" (builtins.toJSON eval.config.assertions);
      serviceJson = pkgs.writeText "service.json" (builtins.toJSON (eval.config.systemd.services or { }));
    in
    pkgs.runCommand "restic-repo-envfile-ok" { buildInputs = [ pkgs.jq pkgs.coreutils ]; } ''
      cat ${assertionsJson} > $out
      if jq -e 'map(.assertion == false) | any' "$out" >/dev/null; then
        echo "Unexpected failed restic assertions with repository + environmentFile" >&2
        cat "$out" >&2
        exit 1
      fi
      jq -e '."restic-backups-ok".environment.RESTIC_REPOSITORY=="s3:https://example.invalid"' ${serviceJson} >/dev/null
      jq -e '."restic-backups-ok".environment.RESTIC_REPOSITORY_FILE==null' ${serviceJson} >/dev/null
      exit 0
    '';
}
