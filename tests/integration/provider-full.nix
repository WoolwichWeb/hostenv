{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  hostenvInput =
    let
      outPath = ../../modules;
    in
    {
      inherit outPath;
      modules = outPath;
      __toString = self: toString self.outPath;
    };

  localSystem = pkgs.stdenv.hostPlatform.system;

  mkProjectInput = { path, organisation, project }:
    let
      projectDir = path + "/.hostenv";
      modules = [
        (projectDir + /hostenv.nix)
        ({ ... }: {
          hostenv = {
            inherit organisation project;
            hostenvHostname = "hosting.test";
            root = path;
            backupsRepoHost = "s3:https://backups.invalid";
          };
        })
      ];
      eval = makeHostenv modules null;
      sanitisedEnvs = lib.mapAttrs (_: env: env // {
        hostenv = env.hostenv // { root = "/src/${project}"; };
      }) eval.config.environments;
    in
    {
      eval = eval;
      input = {
        lib = {
          hostenv = {
            environments = sanitisedEnvs;
            defaultEnvironment = eval.config.defaultEnvironment;
            ${localSystem} = {
              environments = sanitisedEnvs;
              defaultEnvironment = eval.config.defaultEnvironment;
            };
          };
        };
        outPath = projectDir;
        __toString = self: toString self.outPath;
      };
    };

  drupal = mkProjectInput { path = ./drupal; organisation = "acme"; project = "drupal"; };
  drupal7 = mkProjectInput { path = ./drupal7; organisation = "acme"; project = "drupal7"; };

  nodesStub = pkgs.runCommand "nodes-stub" { } ''mkdir -p $out'';
  secretsStub = pkgs.runCommand "secrets-stub" { } ''echo "{}" > $out'';
  stateStub = pkgs.writers.writeJSON "state-stub.json" { };

  inputs = {
    hostenv = hostenvInput;
    acme__drupal = drupal.input;
    acme__drupal7 = drupal7.input;
  };

  lockData = {
    nodes = {
      hostenv = { original = { type = "path"; path = toString ../../modules; }; };
      acme__drupal = { original = { type = "git"; url = "https://example.invalid/drupal.git"; dir = "."; ref = "main"; }; };
      acme__drupal7 = { original = { type = "git"; url = "https://example.invalid/drupal7.git"; dir = "."; ref = "main"; }; };
    };
  };
  lockPath = pkgs.writers.writeJSON "flake.lock" lockData;

  planEval = import ../../provider/plan.nix {
    inherit inputs pkgs lib;
    system = localSystem;
    letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
    deployPublicKeys = [ "ssh-ed25519 test" ];
    hostenvHostname = "hosting.test";
    nodeFor = { default = "node-a"; production = "node-a"; testing = "node-a"; development = "node-a"; };
    statePath = stateStub;
    lockPath = lockPath;
    nodeSystems = { };
    cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
  };

  planData = lib.importJSON planEval.plan;
  flakeText = builtins.readFile planEval.flake;

  drupalEnvUsers = builtins.attrValues (lib.mapAttrs (_: v: v.hostenv.userName) drupal.eval.config.environments);
  drupal7EnvUsers = builtins.attrValues (lib.mapAttrs (_: v: v.hostenv.userName) drupal7.eval.config.environments);
  expectedUsers = drupalEnvUsers ++ drupal7EnvUsers;

  envsPresent = lib.all (u: planData.environments ? ${builtins.toString u}) expectedUsers;
  nodeUsersPresent = lib.all (u: planData.nodes."node-a".users.users ? ${builtins.toString u}) expectedUsers;
  uidsUnique =
    let
      uids = map (u: planData.environments.${u}.uid) expectedUsers;
    in
    lib.length uids == lib.length (lib.unique uids);
  uidsMin = lib.all (u: planData.environments.${u}.uid >= 1001) expectedUsers;
  reposOk = lib.all
    (u:
      let env = planData.environments.${u}; in
      (env.repo.ref or "") == (env.hostenv.gitRef or "")
    )
    expectedUsers;
  flakeInputsPresent =
    lib.all (u: lib.strings.hasInfix "${u} =" flakeText) expectedUsers
    && lib.strings.hasInfix "parent.url" flakeText
      && lib.strings.hasInfix "hostenv.follows = \"parent/hostenv-platform\"" flakeText
      && lib.strings.hasInfix "inputs.parent.lib.provider.deployOutputs" flakeText;
in
{
  provider_full_env_discovery =
    asserts.assertTrue "provider-full-env-discovery" envsPresent
      "plan should include all environments from project inputs";

  provider_full_node_users =
    asserts.assertTrue "provider-full-node-users" nodeUsersPresent
      "node-a should contain users for every environment";

  provider_full_uids =
    asserts.assertTrue "provider-full-uids" (uidsUnique && uidsMin)
      "uids must be unique and >= 1001";

  provider_full_repos =
    asserts.assertTrue "provider-full-repos" reposOk
      "environment repos should carry refs from lock data";

  provider_full_flake_inputs =
    asserts.assertTrue "provider-full-flake-inputs" flakeInputsPresent
      "generated flake should expose hostenv and per-environment inputs";
}
