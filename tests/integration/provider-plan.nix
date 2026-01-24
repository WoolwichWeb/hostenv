{ pkgs, makeHostenv, inputs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;
  providerView = support.providerView;
  providerPlan = inputs.self.lib.provider.plan;

  mkHostenvStub = system:
    let outPath = ../../modules;
    in {
      inherit outPath;
      modules = outPath;
      makeHostenv.${system} = makeHostenv;
      __toString = self: toString outPath;
    };

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
    lib = {
      hostenv = {
        "${"x86_64-linux"}" = {
          environments = { };
          defaultEnvironment = "main";
        };
      };
    };
  };

  dummyStatePath = pkgs.writers.writeJSON "dummy-state.json" { };

  mkPlan = {
    hostenvHostname ? "custom.host",
    state ? { },
    planSource ? "eval",
    planPath ? null,
    deployPublicKeys ? [ "ssh-ed25519 test" ],
    nodeModules ? [ ],
    generatedFlake ? { }
  }:
    let
      # Build a synthetic flake inputs set: hostenv modules + one project with lib.hostenv output.
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

      inputsEffective = {
        hostenv = mkHostenvStub "x86_64-linux";
        acme__ignored = ignoredInput;
        org__proj = {
          outPath = projectDir;
          __toString = self: toString projectDir;
          lib = { hostenv = hostenvOutput; };
        };
      };
    in
    providerPlan {
      inputs = inputsEffective;
      system = "x86_64-linux";
      inherit lib pkgs hostenvHostname;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKeys = deployPublicKeys;
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      statePath = statePathEffective;
      planPath = planPath;
      lockPath = lockPath;
      nodeSystems = { };
      cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
      planSource = planSource;
      inherit nodeModules generatedFlake;
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
        lib = {
          hostenv = {
            "${"x86_64-linux"}" = {
              environments = eval.config.environments;
              defaultEnvironment = eval.config.defaultEnvironment;
            };
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
      ${user1} = { uid = 2001; node = "node1"; virtualHosts = [ "env1.example" "alias.example" ]; };
    };
  }).plan;
  planDisk =
    mkPlan {
      planSource = "disk";
      planPath = planNoState;
      state = lib.importJSON stateNoState;
    };
  planCustom =
    mkPlan {
      nodeModules = [ "nodes/common.nix" ];
      generatedFlake = {
        inputs = {
          extraInput = { url = "github:example/extra"; };
        };
        envInputs = {
          follows = { nixpkgs = "parent/custom-nixpkgs"; };
          extra = _env: {
            inputs = { sops-nix = { follows = "parent/custom-sops-nix"; }; };
          };
        };
      };
    };
  customFlakeText = builtins.readFile planCustom.flake;

  quotedInvalidProject = mkProjectInput { organisation = "acme"; project = "4demo"; envName = "main"; };
  quotedValidProject = mkProjectInput { organisation = "acme"; project = "demo-project"; envName = "main"; };

  quotedInputs = {
    hostenv = mkHostenvStub "x86_64-linux";
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
  quotedPlan = providerPlan {
    inputs = quotedInputs;
    system = "x86_64-linux";
    inherit lib pkgs;
    letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
    deployPublicKeys = [ "ssh-ed25519 test" ];
    hostenvHostname = "hosting.test";
    nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
    statePath = dummyStatePath;
    planPath = null;
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
        hostenv = mkHostenvStub "x86_64-linux";
      };
      lockPath = pkgs.writers.writeJSON "flake.lock" { nodes = { }; };
      result = builtins.tryEval (providerPlan {
        inputs = minimalInputs;
        system = "x86_64-linux";
        inherit lib pkgs;
        letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
        deployPublicKeys = [ "ssh-ed25519 test" ];
        hostenvHostname = "custom.host";
        nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
        statePath = dummyStatePath;
        planPath = null;
        lockPath = lockPath;
        nodeSystems = { };
        cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
        planSource = "eval";
      });
    in result;

  planMissingEnvironments =
    let
      badInputs = {
        hostenv = mkHostenvStub "x86_64-linux";
        org__proj = {
          outPath = projectDir;
          __toString = self: toString projectDir;
          lib = {
            hostenv = {
              x86_64-linux = {
                defaultEnvironment = "main";
                # environments intentionally missing
              };
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
      result = builtins.tryEval (providerPlan {
        inputs = badInputs;
        system = "x86_64-linux";
        inherit lib pkgs;
        letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
        deployPublicKeys = [ "ssh-ed25519 test" ];
        hostenvHostname = "custom.host";
        nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
        statePath = dummyStatePath;
        planPath = null;
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
    mkdir -p $out
    cp ${selfLockFile} $out/flake.lock
  '';

  planDefaultLock =
    builtins.tryEval (providerPlan {
      inputs = {
        self = selfLockDir;
        hostenv = mkHostenvStub "x86_64-linux";
        org__proj = {
          outPath = projectDir;
          __toString = self: toString projectDir;
          lib = { hostenv = hostenvOutput; };
        };
      };
      system = "x86_64-linux";
      inherit lib pkgs;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKeys = [ "ssh-ed25519 test" ];
      hostenvHostname = "custom.host";
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      statePath = dummyStatePath;
      planPath = null;
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
        hostenv = mkHostenvStub "x86_64-linux";
        org__proj = {
          outPath = conflictProjectDir;
          __toString = self: toString conflictProjectDir;
          lib = { hostenv = hostenvOutputConflict; };
        };
      };

      envsExpr = (providerPlan {
        inputs = inputsConflict;
        system = "x86_64-linux";
        inherit lib pkgs;
        letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
        deployPublicKeys = [ "ssh-ed25519 test" ];
        hostenvHostname = "custom.host";
        nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
        statePath = dummyStatePath;
        planPath = null;
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
      providerCfg = plan.nodes.node1.provider or { };
      vhosts = plan.nodes.node1.services.nginx.virtualHosts or { };
      ok = (users ? ${user1}) && (users ? ${user2})
        && (vhosts ? "env1.example") && (vhosts ? "env2.example")
        && (providerCfg.deployPublicKeys or [ ]) == [ "ssh-ed25519 test" ];
    in asserts.assertTrue "provider-plan-node-merge" ok
      "node1 should contain users and vhosts for both environments";

  provider-plan-no-deploy-keys-in-envs =
    let
      plan = lib.importJSON planNoState;
      users = plan.nodes.node1.users.users or { };
      user1Keys = users.${user1}.openssh.authorizedKeys.keys or [ ];
      user2Keys = users.${user2}.openssh.authorizedKeys.keys or [ ];
      ok = !(lib.elem "ssh-ed25519 test" user1Keys) && !(lib.elem "ssh-ed25519 test" user2Keys);
    in asserts.assertTrue "provider-plan-no-deploy-keys-in-envs" ok
      "deploy keys should not be appended to environment users";

  provider-plan-no-deploy-user =
    let
      plan = lib.importJSON planNoState;
      ok = !(plan.nodes.node1.users.users ? deploy);
    in asserts.assertTrue "provider-plan-no-deploy-user" ok
      "plan.json should not define the deploy user";

  provider-plan-alias-preserved =
    let plan = lib.importJSON planWithState;
    in asserts.assertTrue "provider-plan-alias-preserved"
      (plan.environments.${user1}.virtualHosts ? "alias.example")
      "aliases from state should be preserved";

  provider-plan-previous-node =
    let
      plan = lib.importJSON planWithState;
      prev = plan.environments.${user1}.previousNode or null;
    in asserts.assertTrue "provider-plan-previous-node"
      (prev == "node1")
      "previousNode should be carried over from state when present";

  provider-plan-flake-inputs =
    let flakeText = builtins.readFile flakeNoState;
      ok = lib.strings.hasInfix "parent = {" flakeText
        && lib.strings.hasInfix "url = \"path:..\"" flakeText
        && lib.strings.hasInfix "hostenv = {" flakeText
        && lib.strings.hasInfix "follows = \"parent/hostenv\"" flakeText
        && lib.strings.hasInfix "inputs.parent.lib.provider.deployOutputs" flakeText
        && (lib.strings.hasInfix "${user1} =" flakeText || lib.strings.hasInfix "\"${user1}\" =" flakeText)
        && (lib.strings.hasInfix "${user2} =" flakeText || lib.strings.hasInfix "\"${user2}\" =" flakeText);
    in asserts.assertTrue "provider-plan-flake-inputs" ok
      "generated flake should expose hostenv and per-environment inputs";

  provider-plan-flake-customization =
    let
      ok = lib.strings.hasInfix "extraInput = {" customFlakeText
        && lib.strings.hasInfix "custom-nixpkgs" customFlakeText
        && lib.strings.hasInfix "custom-sops-nix" customFlakeText
        && lib.strings.hasInfix "(inputs.parent + \"/nodes/common.nix\")" customFlakeText
        && !(lib.strings.hasInfix "follows = \"hostenv\"" customFlakeText);
    in asserts.assertTrue "provider-plan-flake-customization" ok
      "generated flake should apply extra inputs, env input overrides, and nodeModules";

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
      "plan generation must fail early when no client projects expose outputs.lib.hostenv";

  provider-plan-missing-environments-asserts =
    asserts.assertTrue "provider-plan-missing-environments-asserts"
      (! planMissingEnvironments.success)
      "plan generation must fail early when a client flake lib.hostenv output lacks environments";

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
