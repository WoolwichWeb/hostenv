{ pkgs }:
let
  lib = pkgs.lib;

  mkPlan = { hostenvHostname ? "custom.host", state ? {}, lockData ? {}, projects ? null }:
    (import ../src/provider/plan.nix {
      inputs = {};
      system = "x86_64-linux";
      inherit lib pkgs hostenvHostname;
      letsEncrypt = { adminEmail = "ops@example.test"; acceptTerms = true; };
      deployPublicKey = "ssh-ed25519 test";
      nodeFor = { default = "node1"; production = "node1"; testing = "node1"; development = "node1"; };
      nodesPath = ./.;
      secretsPath = ./.;
      statePath = ./dummy-state.json;
      nodeSystems = {};
      cloudflare = { enable = false; zoneId = null; apiTokenFile = null; };
      testLockData = lockData;
      testState = state;
      testProjects = projects;
    }).plan;

  sampleProjects = [
    {
      hostenv = {
        organisation = "org";
        project = "proj";
        userName = "env1";
        hostname = "env1.example";
        gitRef = "main";
        environmentName = "main";
        hostenvHostname = "ignored.example";
        root = ./.;
      };
      node = "node1";
      authorizedKeys = [];
      type = "development";
      users = {};
      virtualHosts = {
        "env1.example" = { enableLetsEncrypt = true; globalRedirect = null; locations = {}; };
        "alias.example" = { enableLetsEncrypt = true; globalRedirect = null; locations = {}; };
      };
      repo = { type = "git"; dir = "."; ref = "main"; url = "https://example.invalid"; owner = ""; repo = ""; };
    }
    {
      hostenv = {
        organisation = "org";
        project = "proj";
        userName = "env2";
        hostname = "env2.example";
        gitRef = "dev";
        environmentName = "dev";
        hostenvHostname = "ignored.example";
        root = ./.;
      };
      node = "node1";
      authorizedKeys = [];
      type = "development";
      users = {};
      virtualHosts = {
        "env2.example" = { enableLetsEncrypt = true; globalRedirect = null; locations = {}; };
      };
      repo = { type = "git"; dir = "."; ref = "dev"; url = "https://example.invalid"; owner = ""; repo = ""; };
    }
  ];

in let
  planNoState = mkPlan { projects = sampleProjects; };
  planWithState = mkPlan {
    projects = sampleProjects;
    state = { env1 = { uid = 2001; virtualHosts = [ "env1.example" "alias.example" ]; }; };
  };
in {
  provider-plan-regressions = pkgs.runCommand "provider-plan-regressions" { buildInputs = [ pkgs.jq ]; } ''
    set -euo pipefail
    plan=$(mktemp)
    cp ${planNoState} "$plan"

    # Hostname propagation
    jq -e '.hostenvHostname=="custom.host"' "$plan" > /dev/null

    # UID uniqueness and extras present
    uid1=$(jq -r '.environments.env1.uid' "$plan")
    uid2=$(jq -r '.environments.env2.uid' "$plan")
    [ "$uid1" != null ] && [ "$uid2" != null ] || { echo "missing uid"; exit 1; }
    [ "$uid1" -ne "$uid2" ] || { echo "uid collision"; exit 1; }
    jq -e ".environments.env1.extras.uid == $uid1" "$plan" > /dev/null

    # Node merge: both envs should appear under the same node
    jq -e '.nodes.node1.users.users | has("env1") and has("env2")' "$plan" > /dev/null
    jq -e '.nodes.node1.services.nginx.virtualHosts | has("env1.example") and has("env2.example")' "$plan" > /dev/null

    # Alias preservation even with existing state for this env
    plan2=$(mktemp)
    cp ${planWithState} "$plan2"
    jq -e '.environments.env1.virtualHosts | has("alias.example")' "$plan2" > /dev/null

    echo ok > $out
  '';
}
