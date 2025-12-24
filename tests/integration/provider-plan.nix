{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;
  providerView = support.providerView;

  # Shared hostenv eval for tests.
  envsEval = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "org";
        project = "proj";
        hostenvHostname = "hosting.test";
        root = "/srv/fake";
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
            security = {
              csp = "default-src 'self'";
              cspMode = "report-only";
              cspReportTo = "csp-endpoint";
              reportTo = "{\"group\":\"csp-endpoint\",\"max_age\":10886400,\"endpoints\":[{\"url\":\"https://example.invalid/csp\"}]}";
            };
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

  hostenvOutput = {
    "${"x86_64-linux"}" = {
      inherit (envsEval.config) environments defaultEnvironment;
    };
  };

  projectDir = pkgs.runCommand "hostenv-project" { } ''
    mkdir -p $out
    cat > $out/hostenv.nix <<'EOF'
    { pkgs, config, ... }: {
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
            security = {
              csp = "default-src 'self'";
              cspMode = "report-only";
              cspReportTo = "csp-endpoint";
              reportTo = "{\"group\":\"csp-endpoint\",\"max_age\":10886400,\"endpoints\":[{\"url\":\"https://example.invalid/csp\"}]}";
            };
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
      hostenv = {
        organisation = "org";
        project = "proj";
        hostenvHostname = "hosting.test";
        root = "/srv/fake";
      };
    }
    EOF
  '';

  ignoredInputDir = pkgs.runCommand "hostenv-ignored-input" { } ''
    mkdir -p $out
  '';

  ignoredInput = {
    outPath = ignoredInputDir;
    __toString = self: toString self.outPath;
    hostenv = {
      "${"x86_64-linux"}" = {
        environments = { };
        defaultEnvironment = "main";
      };
    };
  };

  dummyStatePath = pkgs.writers.writeJSON "dummy-state.json" { };
  planEmptyPath = ../support/provider/plan-empty.json;

  mkSelfDir = { state, plan ? planEmptyPath }:
    pkgs.runCommand "provider-self" { inherit state plan; } ''
      mkdir -p $out/generated
      cp ${state} $out/generated/state.json
      cp ${plan} $out/generated/plan.json
    '';

  baseSelfDir = mkSelfDir { state = dummyStatePath; };

  mkPlan = { hostenvHostname ? "custom.host", state ? { }, planSource ? "eval", deployPublicKey ? "ssh-ed25519 test", warnInvalidDeployKey ? true, selfDirOverride ? null }:
    let
      # Build a synthetic flake inputs set: hostenv modules + one project with hostenv output.
      lockData = {
        nodes = {
          "org__proj" = {
            original = {
              type = "git";
              url = "https://example.invalid/org/proj.git";
              ref = "main";
            };
            locked = {
              ref = "main";
              rev = "0000000000000000000000000000000000000000";
              narHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            };
          };
        };
      };

      lockPath = pkgs.writers.writeJSON "flake.lock" lockData;

      statePathEffective =
        if state == { }
        then dummyStatePath
        else pkgs.writers.writeJSON "state.json" state;

      selfDir =
        if selfDirOverride != null
        then selfDirOverride
        else mkSelfDir { state = statePathEffective; };

      inputsEffective = {
        self = selfDir;
        hostenv =
          let outPath = ../../modules;
          in {
            inherit outPath;
            modules = outPath;
            __toString = self: toString outPath;
          };
        acme__ignored = ignoredInput;
        org__proj = {
          outPath = projectDir;
          __toString = self: toString projectDir;
          hostenv = hostenvOutput;
        };
      };
    in
    import ../../provider/plan.nix {
      inputs = inputsEffective;
      system = "x86_64-linux";
      inherit lib pkgs hostenvHostname;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKey = deployPublicKey;
      warnInvalidDeployKey = warnInvalidDeployKey;
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      lockPath = lockPath;
      nodeSystems = { };
      cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
      planSource = planSource;
    };

  mkProjectInput = { organisation, project, envName ? "main" }:
    let
      projectDir = pkgs.runCommand "hostenv-project-${project}" { } ''
        mkdir -p $out
        cat > $out/hostenv.nix <<'EOF'
        { pkgs, config, ... }: {
          defaultEnvironment = "${envName}";
          environments.${envName} = {
            enable = true;
            type = "development";
            virtualHosts."${project}-${envName}.example" = {
              enableLetsEncrypt = true;
              globalRedirect = null;
              locations = { };
            };
          };
          hostenv = {
            organisation = "${organisation}";
            project = "${project}";
            hostenvHostname = "hosting.test";
            root = "/srv/${project}";
          };
        }
        EOF
      '';
      eval = makeHostenv [ (projectDir + /hostenv.nix) ] null;
      input = {
        outPath = projectDir;
        __toString = self: toString projectDir;
        hostenv = {
          "${"x86_64-linux"}" = {
            environments = eval.config.environments;
            defaultEnvironment = eval.config.defaultEnvironment;
          };
        };
      };
    in
    {
      inherit projectDir eval input;
    };

  providerEnvs = envsEval.config.environments;

  sampleProjects =
    providerView.toProjects {
      envs = providerEnvs;
      baseRepo = {
        type = "git";
        dir = ".";
        ref = "main";
        url = "https://example.invalid";
        owner = "";
        repo = "";
      };
      node = "node1";
      hostenvHostname = "ignored.example";
      rootBase = "/src";
    };

  user1 = (lib.head sampleProjects).hostenv.userName;
  user2 = (lib.head (lib.tail sampleProjects)).hostenv.userName;

  evalRun = mkPlan { };
  planNoState = evalRun.plan;
  stateNoState = evalRun.state;
  flakeNoState = evalRun.flake;
  planWithState = (mkPlan {
    state = {
      ${user1} = { uid = 2001; virtualHosts = [ "env1.example" "alias.example" ]; };
    };
  }).plan;
  planDisk =
    mkPlan {
      planSource = "disk";
      selfDirOverride = mkSelfDir { state = stateNoState; plan = planNoState; };
    };
  invalidDeployKey = "not-a-key";
  planInvalidDeployKey = builtins.tryEval (mkPlan { deployPublicKey = invalidDeployKey; warnInvalidDeployKey = false; });

  quotedInvalidProject = mkProjectInput { organisation = "acme"; project = "4demo"; envName = "main"; };
  quotedValidProject = mkProjectInput { organisation = "acme"; project = "demo-project"; envName = "main"; };

  quotedInputs = {
    self = baseSelfDir;
    hostenv =
      let outPath = ../../modules;
      in {
        inherit outPath;
        modules = outPath;
        __toString = self: toString outPath;
      };
    acme__4demo = quotedInvalidProject.input;
    acme__demo-project = quotedValidProject.input;
  };

  quotedLockData = {
    nodes = {
      "acme__4demo" = {
        original = {
          type = "git";
          url = "https://example.invalid/4demo.git";
          ref = "main";
        };
      };
      "acme__demo-project" = {
        original = {
          type = "git";
          url = "https://example.invalid/demo-project.git";
          ref = "main";
        };
      };
    };
  };

  quotedLockPath = pkgs.writers.writeJSON "flake.lock" quotedLockData;
  quotedPlan = import ../../provider/plan.nix {
    inputs = quotedInputs;
    system = "x86_64-linux";
    inherit lib pkgs;
    letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
    deployPublicKey = "ssh-ed25519 test";
    hostenvHostname = "hosting.test";
    nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
    lockPath = quotedLockPath;
    nodeSystems = { };
    cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
    planSource = "eval";
  };
  quotedFlakeText = builtins.readFile quotedPlan.flake;
  quotedInvalidUser = quotedInvalidProject.eval.config.environments.main.hostenv.userName;
  quotedValidUser = quotedValidProject.eval.config.environments.main.hostenv.userName;
  planMissingProjects =
    let
      minimalInputs = {
        self = baseSelfDir;
        hostenv =
          let outPath = ../../modules;
          in { inherit outPath; modules = outPath; __toString = self: toString outPath; };
      };
      lockPath = pkgs.writers.writeJSON "flake.lock" { nodes = { }; };
      result = builtins.tryEval (import ../../provider/plan.nix {
        inputs = minimalInputs;
        system = "x86_64-linux";
        inherit lib pkgs;
        letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
        deployPublicKey = "ssh-ed25519 test";
        hostenvHostname = "custom.host";
        nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
        lockPath = lockPath;
        nodeSystems = { };
        cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
        planSource = "eval";
      });
    in result;

  planMissingEnvironments =
    let
      badInputs = {
        self = baseSelfDir;
        hostenv =
          let outPath = ../../modules;
          in { inherit outPath; modules = outPath; __toString = self: toString outPath; };
        org__proj = {
          outPath = projectDir;
          __toString = self: toString projectDir;
          hostenv = {
            x86_64-linux = {
              defaultEnvironment = "main";
              # environments intentionally missing
            };
          };
        };
      };
      lockData = {
        nodes = {
          "org__proj" = {
            original = {
              type = "git";
              url = "https://example.invalid/org/proj.git";
              ref = "main";
            };
            locked = {
              ref = "main";
              rev = "0000000000000000000000000000000000000000";
              narHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            };
          };
        };
      };
      lockPath = pkgs.writers.writeJSON "flake.lock" lockData;
      result = builtins.tryEval (import ../../provider/plan.nix {
        inputs = badInputs;
        system = "x86_64-linux";
        inherit lib pkgs;
        letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
        deployPublicKey = "ssh-ed25519 test";
        hostenvHostname = "custom.host";
        nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
        lockPath = lockPath;
        nodeSystems = { };
        cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
        planSource = "eval";
      });
    in result;

  lockDataSelf = {
    nodes = {
      "org__proj" = {
        original = {
          type = "git";
          url = "https://example.invalid/org/proj.git";
          ref = "main";
        };
        locked = {
          ref = "main";
          rev = "0000000000000000000000000000000000000000";
          narHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        };
      };
    };
  };

  selfLockFile = pkgs.writers.writeJSON "flake.lock" lockDataSelf;
  selfLockDir = pkgs.runCommand "provider-self-lock" { } ''
    mkdir -p $out/generated
    cp ${selfLockFile} $out/flake.lock
    cp ${dummyStatePath} $out/generated/state.json
    cp ${planEmptyPath} $out/generated/plan.json
  '';

  planDefaultLock =
    builtins.tryEval (import ../../provider/plan.nix {
      inputs = {
        self = selfLockDir;
        hostenv =
          let outPath = ../../modules;
          in { inherit outPath; modules = outPath; __toString = self: toString outPath; };
        org__proj = {
          outPath = projectDir;
          __toString = self: toString projectDir;
          hostenv = hostenvOutput;
        };
      };
      system = "x86_64-linux";
      inherit lib pkgs;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKey = "ssh-ed25519 test";
      hostenvHostname = "custom.host";
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      nodeSystems = { };
      cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
      planSource = "eval";
    });

  providerPlanVhostConflictState =
    let
      conflictState = {
        other = { uid = 2500; virtualHosts = [ "env1.example" ]; };
      };
      envsExpr = (mkPlan { state = conflictState; }).environments;
      result = builtins.tryEval (builtins.deepSeq envsExpr envsExpr);
    in asserts.assertTrue "provider-plan-vhost-conflict-state"
      (! result.success)
      "plan generation must fail when virtualHosts overlap with existing state";

  providerPlanVhostConflictNewEnvs =
    let
      conflictProjectDir = pkgs.runCommand "hostenv-project-conflict" { } ''
        mkdir -p $out
        cat > $out/hostenv.nix <<'EOF'
        { pkgs, config, ... }: {
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
            };
            env2 = {
              enable = true;
              type = "development";
              hostenv.userName = "env2";
              hostenv.hostname = "env2.example";
              virtualHosts."env1.example" = {
                enableLetsEncrypt = true;
                globalRedirect = null;
                locations = { };
              };
            };
          };
          hostenv = {
            organisation = "org";
            project = "proj";
            hostenvHostname = "hosting.test";
            root = "/srv/fake";
          };
        }
EOF
      '';

      conflictEnvEval = makeHostenv [ "${conflictProjectDir}/hostenv.nix" ] null;

      hostenvOutputConflict = {
        "${"x86_64-linux"}" = {
          inherit (conflictEnvEval.config) environments defaultEnvironment;
        };
      };

      lockData = {
        nodes = {
          "org__proj" = {
            original = {
              type = "git";
              url = "https://example.invalid/org/proj.git";
              ref = "main";
            };
            locked = {
              ref = "main";
              rev = "0000000000000000000000000000000000000000";
              narHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            };
          };
        };
      };
      lockPath = pkgs.writers.writeJSON "flake.lock" lockData;

      inputsConflict = {
        self = baseSelfDir;
        hostenv =
          let outPath = ../../modules;
          in { inherit outPath; modules = outPath; __toString = self: toString outPath; };
        org__proj = {
          outPath = conflictProjectDir;
          __toString = self: toString conflictProjectDir;
          hostenv = hostenvOutputConflict;
        };
      };

      envsExpr = (import ../../provider/plan.nix {
        inputs = inputsConflict;
        system = "x86_64-linux";
        inherit lib pkgs;
        letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
        deployPublicKey = "ssh-ed25519 test";
        hostenvHostname = "custom.host";
        nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
        lockPath = lockPath;
        nodeSystems = { };
        cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
        planSource = "eval";
      }).environments;
      result = builtins.tryEval (builtins.deepSeq envsExpr envsExpr);
    in asserts.assertTrue "provider-plan-vhost-conflict-new-envs"
      (! result.success)
      "plan generation must fail when virtualHosts overlap between new environments";
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
      present = lib.all (u: u != null) uids;
    in asserts.assertTrue "provider-plan-uids"
      (unique && present)
      "UIDs must be unique and present";

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
      ok = lib.strings.hasInfix "parent.url" flakeText
        && lib.strings.hasInfix "hostenv.follows = \"parent/hostenv\"" flakeText
        && lib.strings.hasInfix "${user1} =" flakeText
        && lib.strings.hasInfix "${user2} =" flakeText;
    in asserts.assertTrue "provider-plan-flake-inputs" ok
      "generated flake should expose hostenv and per-environment inputs";

  provider-plan-flake-inputs-quoted =
    let
      ok = lib.strings.hasInfix "\"${quotedInvalidUser}\" =" quotedFlakeText
        && lib.strings.hasInfix "${quotedValidUser} =" quotedFlakeText
        && !(lib.strings.hasInfix "\"${quotedValidUser}\" =" quotedFlakeText);
    in asserts.assertTrue "provider-plan-flake-inputs-quoted" ok
      "generated flake should quote invalid input identifiers and leave valid ones unquoted";

  provider-plan-planSource-disk =
    let
      evalPlanData = lib.importJSON planNoState;
      diskPlanData = lib.importJSON planDisk.plan;
    in asserts.assertTrue "provider-plan-planSource-disk"
      (evalPlanData == diskPlanData)
      "planSource=\"disk\" should reuse plan.json contents without re-evaluating hostenv";

  provider-plan-missing-projects-asserts =
    asserts.assertTrue "provider-plan-missing-projects-asserts"
      (! planMissingProjects.success)
      "plan generation must fail early when no client projects expose outputs.hostenv";

  provider-plan-missing-environments-asserts =
    asserts.assertTrue "provider-plan-missing-environments-asserts"
      (! planMissingEnvironments.success)
      "plan generation must fail early when a client flake hostenv output lacks environments";

  provider-plan-invalid-deploy-key =
    let
      invalidKeyPresent =
        if planInvalidDeployKey.success then
          let
            plan = lib.importJSON planInvalidDeployKey.value.plan;
            users = plan.nodes.node1.users.users or { };
            keyLists = map (u: u.openssh.authorizedKeys.keys or [ ]) (builtins.attrValues users);
          in
            lib.any (keys: lib.elem invalidDeployKey keys) keyLists
        else
          true;
    in
    asserts.assertTrue "provider-plan-invalid-deploy-key"
      (planInvalidDeployKey.success && !invalidKeyPresent)
      "plan generation should warn and omit invalid deployPublicKey";

  provider-plan-security-headers =
    let
      plan = lib.importJSON planNoState;
      vhost = plan.nodes.node1.services.nginx.virtualHosts."env1.example" or { };
      extraConfig = vhost.extraConfig or "";
      aliasVhost = plan.nodes.node1.services.nginx.virtualHosts."alias.example" or { };
      aliasExtra = aliasVhost.extraConfig or "";
      hasCsp =
        lib.strings.hasInfix "Content-Security-Policy-Report-Only" extraConfig
        && lib.strings.hasInfix "default-src 'self'; report-to csp-endpoint" extraConfig;
      hasReportTo =
        lib.strings.hasInfix "Report-To" extraConfig
        && lib.strings.hasInfix "\"group\":\"csp-endpoint\"" extraConfig;
      noCspOnAlias = !(lib.strings.hasInfix "Content-Security-Policy" aliasExtra);
      hasReferrer = lib.strings.hasInfix "Referrer-Policy" extraConfig;
      hasRobots = lib.strings.hasInfix "X-Robots-Tag" extraConfig;
      hasHsts = lib.strings.hasInfix "Strict-Transport-Security" extraConfig;
    in
    asserts.assertTrue "provider-plan-security-headers"
      (hasCsp && hasReportTo && hasReferrer && hasRobots && hasHsts && noCspOnAlias)
      "plan should emit security headers for configured virtual hosts and omit CSP when unset";

  provider-plan-default-lock =
    asserts.assertTrue "provider-plan-default-lock"
      planDefaultLock.success
      "plan generation should use inputs.self/flake.lock when lockPath is omitted";

  provider-plan-vhost-conflict-state = providerPlanVhostConflictState;
  provider-plan-vhost-conflict-new-envs = providerPlanVhostConflictNewEnvs;
}
