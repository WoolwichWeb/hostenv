{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ../support { inherit pkgs lib; }).asserts;

  baseModule = { ... }: {
    hostenv = {
      organisation = "acme";
      project = "demo";
      hostenvHostname = "hosting.test";
      root = ./.;
    };
    allEnvironments.users.alice = {
      email = "alice@example.com";
      publicKeys = [ ];
      gitlabUsername = "AliceUser";
    };
    environments.main = {
      enable = true;
      type = "testing";
    };
  };

  providerDisabledEval = builtins.tryEval (
    (makeHostenv [
      baseModule
      ({ ... }: {
        services.hostenv-provider.enable = false;
        services.hostenv-provider.gitlab.enable = false;
      })
    ] null).config.activate
  );

  gitlabDisabledEval = builtins.tryEval (
    (makeHostenv [
      baseModule
      ({ ... }: {
        services.hostenv-provider.enable = true;
        services.hostenv-provider.gitlab.enable = false;
      })
    ] null).config.activate
  );

  conflictEval = builtins.tryEval (
    (makeHostenv [
      ({ ... }: {
        hostenv = {
          organisation = "acme";
          project = "demo";
          hostenvHostname = "hosting.test";
          root = ./.;
        };
        services.hostenv-provider.enable = true;
        services.hostenv-provider.gitlab.enable = true;
        environments.main = {
          enable = true;
          type = "testing";
          users.alice = {
            email = "alice@example.com";
            publicKeys = [ ];
            gitlabUsername = "alice_main";
          };
        };
        environments.dev = {
          enable = true;
          type = "development";
          users.alice = {
            email = "alice@example.com";
            publicKeys = [ ];
            gitlabUsername = "alice_dev";
          };
        };
      })
    ] null).config.activate
  );

  seededEnv = makeHostenv [
    baseModule
    ({ ... }: {
      services.hostenv-provider.enable = true;
      services.hostenv-provider.gitlab.enable = true;
      services.hostenv-provider.gitlab.hosts = [ "GitLab.com" "gitlab.example.com" ];
    })
  ] null;

  execStart = seededEnv.config.systemd.services.hostenv-provider.serviceConfig.ExecStart or "";
  startScript = if execStart == "" then "" else builtins.readFile execStart;
  configParts = lib.splitString "--config " startScript;
  hasConfigPath = lib.length configParts > 1;
  configPath =
    if hasConfigPath
    then builtins.head (lib.splitString "\n" (builtins.elemAt configParts 1))
    else "";
  providerConfig =
    if configPath == ""
    then { }
    else builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile configPath));
  seedUsers = providerConfig.seedUsers or [ ];
  aliceRows = lib.filter (u: (u.configUsername or "") == "alice") seedUsers;
  hasAliceSeed = lib.length aliceRows == 1;
  aliceSeed = if hasAliceSeed then builtins.head aliceRows else { };
  aliceAccounts = aliceSeed.providerAccounts or [ ];
  hasGitlabCom = builtins.any (a: (a.provider or "") == "gitlab" && (a.host or "") == "gitlab.com" && (a.username or "") == "aliceuser") aliceAccounts;
  hasGitlabSelfHost = builtins.any (a: (a.provider or "") == "gitlab" && (a.host or "") == "gitlab.example.com" && (a.username or "") == "aliceuser") aliceAccounts;
  hasAdminRole = (aliceSeed.role or "") == "admin";
in
{
  provider-user-seeding-requires-provider-enabled =
    asserts.assertTrue "provider-user-seeding-requires-provider-enabled"
      (!providerDisabledEval.success)
      "hostenv-provider should assert when users.<name>.gitlabUsername is set but services.hostenv-provider.enable is false";

  provider-user-seeding-requires-gitlab-enabled =
    asserts.assertTrue "provider-user-seeding-requires-gitlab-enabled"
      (!gitlabDisabledEval.success)
      "hostenv-provider should assert when users.<name>.gitlabUsername is set but services.hostenv-provider.gitlab.enable is false";

  provider-user-seeding-detects-conflicts =
    asserts.assertTrue "provider-user-seeding-detects-conflicts"
      (!conflictEval.success)
      "hostenv-provider should assert when the same user has conflicting gitlabUsername values across enabled environments";

  provider-user-seeding-config-json =
    asserts.assertTrue "provider-user-seeding-config-json"
      (hasAliceSeed && hasGitlabCom && hasGitlabSelfHost && hasAdminRole)
      "hostenv-provider config JSON should include normalized seedUsers/providerAccounts for GitLab";
}
