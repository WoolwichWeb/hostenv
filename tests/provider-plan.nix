{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ./support { inherit pkgs lib; };
  asserts = support.asserts;
  providerView = support.providerView;

  # Build sample environments from a real hostenv eval, then project them into
  # the provider shape via providerView (keeps schema consistent with production).
  sampleProjects =
    let
      envsEval = makeHostenv [
        ({ ... }: {
          hostenv = {
            organisation = "org";
            project = "proj";
            hostenvHostname = "hosting.test";
            root = ./.;
          };
          defaultEnvironment = "env1";
          environments = {
            env1 = {
              enable = true;
              type = "development";
              hostenv.userName = "env1";
              hostenv.hostname = "env1.example";
              virtualHosts."env1.example" = {
                enableLetsEncrypt = true;
                globalRedirect = null;
                locations = { };
              };
              virtualHosts."alias.example" = {
                enableLetsEncrypt = true;
                globalRedirect = null;
                locations = { };
              };
            };
            env2 = {
              enable = true;
              type = "development";
              hostenv.userName = "env2";
              hostenv.hostname = "env2.example";
              virtualHosts."env2.example" = {
                enableLetsEncrypt = true;
                globalRedirect = null;
                locations = { };
              };
            };
          };
        })
      ] null;
      baseRepo = {
        type = "git";
        dir = ".";
        ref = "main";
        url = "https://example.invalid";
        owner = "";
        repo = "";
      };

      providerEnvs = providerView envsEval.config.environments;
    in
    lib.attrValues (lib.mapAttrs
      (_: envCfg: {
        hostenv = envCfg.hostenv // {
          gitRef = envCfg.hostenv.gitRef or baseRepo.ref;
          hostenvHostname = "ignored.example";
          root = "/src/${envCfg.hostenv.project or "proj"}";
        };
        node = "node1";
        authorizedKeys =
          let allUsers = builtins.attrValues (envCfg.users or { }); in
          builtins.concatLists (map (u: u.publicKeys or [ ]) allUsers);
        type = envCfg.type;
        users = envCfg.users or { };
        virtualHosts = envCfg.virtualHosts;
        repo = baseRepo // { ref = envCfg.hostenv.gitRef or baseRepo.ref; };
      })
      providerEnvs);

  lockData = { nodes = { }; };

  mkPlan = { hostenvHostname ? "custom.host", state ? { }, lockData ? { }, projects ? null, inputsOverride ? { }, planSource ? "eval", planPath ? null }:
    let
      inputsEffective =
        if inputsOverride == { }
        then {
          hostenv =
            let outPath = ../modules;
            in {
              inherit outPath;
              modules = outPath;
              __toString = self: toString outPath;
            };
        }
        else inputsOverride;
    in
    import ../provider/plan.nix {
      inputs = inputsEffective;
      system = "x86_64-linux";
      inherit lib pkgs hostenvHostname;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKey = "ssh-ed25519 test";
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      nodesPath = ./.;
      secretsPath = ./.;
      statePath = ./dummy-state.json;
      planPath = planPath;
      nodeSystems = { };
      cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
      testLockData = lockData;
      testState = state;
      testProjects = projects;
      planSource = planSource;
    };

  user1 = (lib.head sampleProjects).hostenv.userName;
  user2 = (lib.head (lib.tail sampleProjects)).hostenv.userName;

  evalRun = mkPlan { projects = sampleProjects; };
  planNoState = evalRun.plan;
  stateNoState = evalRun.state;
  flakeNoState = evalRun.flake;
  planWithState = (mkPlan {
    projects = sampleProjects;
    state = {
      ${user1} = { uid = 2001; virtualHosts = [ "env1.example" "alias.example" ]; };
    };
  }).plan;
  planDisk =
    mkPlan {
      projects = null;
      planSource = "disk";
      planPath = planNoState;
      state = lib.importJSON stateNoState;
      inherit lockData;
    };
in
{
  provider-plan-hostname =
    let plan = lib.importJSON planNoState;
    in asserts.assertTrue "provider-plan-hostname"
      (plan.hostenvHostname == "custom.host")
      "hostenvHostname should propagate to plan.json";

  provider-plan-uids =
    let
      plan = lib.importJSON planNoState;
      uids = map (u: plan.environments.${u}.uid) [ user1 user2 ];
      unique = (lib.length uids) == (lib.length (lib.unique uids));
      extrasOk = plan.environments.${user1}.extras.uid == plan.environments.${user1}.uid;
    in asserts.assertTrue "provider-plan-uids"
      (unique && extrasOk)
      "UIDs must be unique and extras.uid must mirror uid";

  provider-plan-node-merge =
    let
      plan = lib.importJSON planNoState;
      users = plan.nodes.node1.users.users or { };
      vhosts = plan.nodes.node1.services.nginx.virtualHosts or { };
      ok = (users ? ${user1}) && (users ? ${user2})
        && (vhosts ? "env1.example") && (vhosts ? "env2.example");
    in asserts.assertTrue "provider-plan-node-merge" ok
      "node1 should contain users and vhosts for both environments";

  provider-plan-alias-preserved =
    let plan = lib.importJSON planWithState;
    in asserts.assertTrue "provider-plan-alias-preserved"
      (plan.environments.${user1}.virtualHosts ? "alias.example")
      "aliases from state should be preserved";

  provider-plan-flake-inputs =
    let flakeText = builtins.readFile flakeNoState;
      ok = lib.strings.hasInfix "hostenv.url" flakeText
        && lib.strings.hasInfix "${user1} =" flakeText
        && lib.strings.hasInfix "${user2} =" flakeText;
    in asserts.assertTrue "provider-plan-flake-inputs" ok
      "generated flake should expose hostenv and per-environment inputs";

  provider-plan-vhost-conflict =
    let
      conflictProjects = [
        {
          hostenv = {
            userName = "u1";
            hostname = "u1.hosting.test";
            gitRef = "main";
            hostenvHostname = "hosting.test";
            project = "proj";
            organisation = "org";
            root = ".";
          };
          node = "node1";
          authorizedKeys = [ ];
          type = "development";
          users = { };
          virtualHosts = {
            "conflict.test" = {
              enableLetsEncrypt = true;
              globalRedirect = null;
              locations = { };
            };
          };
          repo = {
            type = "git";
            url = "https://example.invalid";
            dir = ".";
            ref = "main";
            owner = "";
            repo = "";
          };
        }
      ];
      conflictState = {
        other = {
          uid = 2500;
          virtualHosts = [ "conflict.test" ];
        };
      };
      envsExpr = (mkPlan {
        projects = conflictProjects;
        state = conflictState;
      }).environments;
      result = builtins.tryEval (builtins.deepSeq envsExpr envsExpr);
    in asserts.assertTrue "provider-plan-vhost-conflict"
      (!result.success)
      "plan generation must fail when virtualHosts overlap with existing state";

  provider-plan-vhost-conflict-new-envs =
    let
      conflictProjects = [
        {
          hostenv = {
            userName = "u1";
            hostname = "u1.hosting.test";
            gitRef = "main";
            hostenvHostname = "hosting.test";
            project = "proj";
            organisation = "org";
            root = ".";
          };
          node = "node1";
          authorizedKeys = [ ];
          type = "development";
          users = { };
          virtualHosts = {
            "dup.test" = {
              enableLetsEncrypt = true;
              globalRedirect = null;
              locations = { };
            };
          };
          repo = {
            type = "git";
            url = "https://example.invalid";
            dir = ".";
            ref = "main";
            owner = "";
            repo = "";
          };
        }
        {
          hostenv = {
            userName = "u2";
            hostname = "u2.hosting.test";
            gitRef = "main";
            hostenvHostname = "hosting.test";
            project = "proj";
            organisation = "org";
            root = ".";
          };
          node = "node1";
          authorizedKeys = [ ];
          type = "development";
          users = { };
          virtualHosts = {
            "dup.test" = {
              enableLetsEncrypt = true;
              globalRedirect = null;
              locations = { };
            };
          };
          repo = {
            type = "git";
            url = "https://example.invalid";
            dir = ".";
            ref = "main";
            owner = "";
            repo = "";
          };
        }
      ];
      envsExpr = (mkPlan {
        projects = conflictProjects;
        state = { };
      }).environments;
      result = builtins.tryEval (builtins.deepSeq envsExpr envsExpr);
    in asserts.assertTrue "provider-plan-vhost-conflict-new-envs"
      (!result.success)
      "plan generation must fail when virtualHosts overlap between new environments";

  provider-plan-planSource-disk =
    let
      evalPlanData = lib.importJSON planNoState;
      diskPlanData = lib.importJSON planDisk.plan;
    in asserts.assertTrue "provider-plan-planSource-disk"
      (evalPlanData == diskPlanData)
      "planSource=\"disk\" should reuse plan.json contents without re-evaluating hostenv";
}
