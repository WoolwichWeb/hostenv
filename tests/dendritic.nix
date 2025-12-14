{ pkgs, makeHostenv }:

let
  lib = pkgs.lib;
  support = import ./support { inherit pkgs lib; };
  stubs = support.stubs;
  providerView = support.providerView;

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

  upstreamsJson = builtins.toFile "upstreams.json"
    (builtins.toJSON (planBridgeEval.config.services.nginx.upstreams or { }));
  resticJson = builtins.toFile "restic.json"
    (builtins.toJSON (planBridgeEval.config.services.restic.backups or { }));
  resticFullJson = builtins.toFile "restic-full.json"
    (builtins.toJSON (planBridgeEval.config.services.restic or { }));
  hostenvJson = builtins.toFile "hostenv-envs.json"
    (builtins.toJSON (planBridgeEval.config.hostenv.environments or { }));
  providerJson = builtins.toFile "provider-envs.json"
    (builtins.toJSON providerEnvs);
  evalEnvsJson = builtins.toFile "eval-envs.json"
    (builtins.toJSON (planBridgeEval.config.environments or { }));
  backupsEnabled = builtins.toFile "backups-enabled.txt"
    (builtins.toJSON (planBridgeEval.config.hostenv.backups.enable or false));

  upstreamsTest = pkgs.runCommand "dendritic-nginx-disabled-envs"
    { buildInputs = [ pkgs.jq pkgs.coreutils ]; } ''
    cat ${upstreamsJson} > $out
    # Enabled envs: main + test (dev is disabled). Upstream count should be 2.
    count=$(jq 'length' "$out")
    test "$count" -eq 2 || { echo "expected 2 upstreams, got $count"; exit 1; }
  '';

  backupsTest = pkgs.runCommand "dendritic-backups-enabled-only"
    { buildInputs = [ pkgs.jq pkgs.coreutils ]; } ''
    cat ${resticJson} > $out
    # Only main carries backups extras; others should be absent.
    if ! jq -e 'has("main") and (length==1)' "$out" > /dev/null; then
      echo "restic json:" >&2
      cat "$out" >&2
      echo "hostenv environments:" >&2
      cat ${hostenvJson} >&2
      echo "hostenv backups.enable:" >&2
      cat ${backupsEnabled} >&2
      echo "services.restic:" >&2
      cat ${resticFullJson} >&2
      echo "provider environments:" >&2
      cat ${providerJson} >&2
      echo "evaluated environments:" >&2
      cat ${evalEnvsJson} >&2
      exit 1
    fi
    if ! jq -e '.main.paths[] | select(.=="/var/lib/test-data")' "$out" > /dev/null; then
      echo "restic json:" >&2
      cat "$out" >&2
      echo "hostenv environments:" >&2
      cat ${hostenvJson} >&2
      echo "hostenv backups.enable:" >&2
      cat ${backupsEnabled} >&2
      echo "provider environments:" >&2
      cat ${providerJson} >&2
      echo "evaluated environments:" >&2
      cat ${evalEnvsJson} >&2
      exit 1
    fi
  '';

  # Keep the slice regression guard (stubbed options are sufficient).
  sliceEval =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          ({ lib, ... }: {
            options.hostenv = lib.mkOption {
              type = lib.types.submodule {
                freeformType = lib.types.attrs;
                options.defaultEnvironment = lib.mkOption {
                  type = lib.types.str;
                  default = "main";
                };
              };
              default = { };
            };
            options.systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.users = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.groups = lib.mkOption { type = lib.types.attrs; default = { }; };
          })
          ../modules/core/environments.nix
          ../modules/nixos/users-slices.nix
          ({ config, ... }: {
            _module.check = false;
            environments = {
              alpha = {
                _module.check = false;
                enable = true;
                user = "alpha";
                extras.publicKeys = [ ];
                extras.uid = 123;
              };
            };
            hostenv.environments = config.environments;
          })
        ];
      };
      slice = eval.config.systemd.slices."alpha.slice";
      sliceJson = builtins.toFile "slice.json" (builtins.toJSON slice);
    in
    pkgs.runCommand "users-slices-configured" { } ''
      cp ${sliceJson} $out
      grep -q '"CPUAccounting":"yes"' $out
      grep -E '"MemoryMax":"[0-9]+G"' $out
    '';

in
{
  slice_defaults_applied = sliceEval;
  disabled_envs_filtered = upstreamsTest;
  backups_filtered = backupsTest;
  slice_respects_custom_user =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          ({ lib, ... }: {
            options.hostenv = lib.mkOption {
              type = lib.types.submodule { freeformType = lib.types.attrs; };
              default = { };
            };
            options.systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.users = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.groups = lib.mkOption { type = lib.types.attrs; default = { }; };
          })
          ../modules/nixos/users-slices.nix
          ({ ... }: {
            _module.check = false;
            hostenv.environments = {
              envWithCustomUser = {
                enable = true;
                user = "customuser";
                extras.uid = 321;
                extras.publicKeys = [ ];
              };
            };
          })
        ];
      };
      slicesJson = builtins.toFile "slices.json" (builtins.toJSON eval.config.systemd.slices);
      usersJson = builtins.toFile "users.json" (builtins.toJSON eval.config.users.users);
      servicesJson = builtins.toFile "services.json" (builtins.toJSON eval.config.systemd.services);
    in
    pkgs.runCommand "users-slices-custom-user" { buildInputs = [ pkgs.jq pkgs.coreutils ]; } ''
      cat ${slicesJson} > $out
      jq -e 'has("customuser.slice")' ${slicesJson} >/dev/null
      jq -e 'has("customuser")' ${usersJson} >/dev/null
      jq -e '."user@321".serviceConfig.Slice=="customuser.slice"' ${servicesJson} >/dev/null
    '';
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
