{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;

  # Build sample environments from an actual hostenv evaluation to stay
  # aligned with the module defaults.
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
    in
    lib.attrValues (lib.mapAttrs
      (_name: envCfg:
        let
          hostenv' = envCfg.hostenv or { } // {
            gitRef = envCfg.hostenv.gitRef or baseRepo.ref;
            hostenvHostname = "ignored.example";
          };
        in
        {
          hostenv = hostenv';
          node = "node1";
          authorizedKeys = [ ];
          type = envCfg.type;
          users = envCfg.users or { };
          virtualHosts = envCfg.virtualHosts;
          repo = baseRepo // { ref = hostenv'.gitRef; };
        })
      envsEval.config.environments);

  mkPlan = { hostenvHostname ? "custom.host", state ? { }, lockData ? { }, projects ? null, inputsOverride ? { } }:
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
    import ../src/provider/plan.nix {
      inputs = inputsEffective;
      system = "x86_64-linux";
      inherit lib pkgs hostenvHostname;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKey = "ssh-ed25519 test";
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      nodesPath = ./.;
      secretsPath = ./.;
      statePath = ./dummy-state.json;
      nodeSystems = { };
      cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
      testLockData = lockData;
      testState = state;
      testProjects = projects;
    };

  user1 = (lib.head sampleProjects).hostenv.userName;
  user2 = (lib.head (lib.tail sampleProjects)).hostenv.userName;

  planNoState = (mkPlan { projects = sampleProjects; }).plan;
  flakeNoState = (mkPlan { projects = sampleProjects; }).flake;
  planWithState = (mkPlan {
    projects = sampleProjects;
    state = {
      ${user1} = { uid = 2001; virtualHosts = [ "env1.example" "alias.example" ]; };
    };
  }).plan;
in
{
  provider-plan-regressions = pkgs.runCommand "provider-plan-regressions" { buildInputs = [ pkgs.jq pkgs.gnugrep ]; } ''
    set -euo pipefail
    user1="${user1}"
    user2="${user2}"

    plan=$(mktemp)
    cp ${planNoState} "$plan"

    # Hostname propagation
    jq -e '.hostenvHostname=="custom.host"' "$plan" > /dev/null

    # UID uniqueness and extras present
    uid1=$(jq -r ".environments.\"$user1\".uid" "$plan")
    uid2=$(jq -r ".environments.\"$user2\".uid" "$plan")
    [ "$uid1" != null ] && [ "$uid2" != null ] || { echo "missing uid"; exit 1; }
    [ "$uid1" -ne "$uid2" ] || { echo "uid collision"; exit 1; }
    jq -e ".environments.\"$user1\".extras.uid == $uid1" "$plan" > /dev/null

    # Node merge: both envs should appear under the same node
    jq -e ".nodes.node1.users.users | has(\"$user1\") and has(\"$user2\")" "$plan" > /dev/null
    jq -e '.nodes.node1.services.nginx.virtualHosts | has("env1.example") and has("env2.example")' "$plan" > /dev/null

    # Alias preservation even with existing state for this env
    plan2=$(mktemp)
    cp ${planWithState} "$plan2"
    jq -e ".environments.\"$user1\".virtualHosts | has(\"alias.example\")" "$plan2" > /dev/null

    # Generated flake should include hostenv input and both environment inputs
    flake=$(mktemp)
    cp ${flakeNoState} "$flake"
    grep -q 'hostenv.url' "$flake"
    grep -q "$user1 =" "$flake"
    grep -q "$user2 =" "$flake"

    echo ok > $out
  '';
}
