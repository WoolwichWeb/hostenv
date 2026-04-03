{ pkgs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;
  deployAgentModule = import ../../modules/services/hostenv-deploy-agent/module.nix { };
  hostenvDeployAgentModule = (import ../../modules/nixos/hostenv-deploy-agent.nix { }).flake.modules.nixos."hostenv-deploy-agent";

  eval = support.evalWithBase {
    modules = [
      ({ lib, ... }: {
        options.flake = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        options.security = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
      })
      deployAgentModule
      hostenvDeployAgentModule
      ({ ... }: {
        services.hostenv-deploy-agent = {
          enable = true;
          providerApiBaseUrl = "https://hosting.test";
          nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
          nodeName = "node-a";
          reconnectSeconds = 7;
          actionTimeoutSeconds = 900;
        };
      })
    ];
  };

  service = eval.config.systemd.services.hostenv-deploy-agent or { };
  serviceConfig = service.serviceConfig or { };
  execStart = serviceConfig.ExecStart or "";
  execStartParts = lib.splitString " --config " execStart;
  hasConfigPath = lib.length execStartParts == 2;
  configPath = if hasConfigPath then builtins.elemAt execStartParts 1 else "";
  agentConfig =
    if configPath == ""
    then { }
    else builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile configPath));
  configMatches =
    agentConfig == {
      providerApiBaseUrl = "https://hosting.test";
      nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
      nodeName = "node-a";
      stateFile = "/var/lib/hostenv-deploy-agent/state.json";
      actionTimeoutSeconds = 900;
      reconnectSeconds = 7;
    };
  serviceStable =
    (serviceConfig.User or "") == "root"
    && (serviceConfig.StateDirectory or "") == "hostenv-deploy-agent"
    && lib.hasInfix "/bin/hostenv-deploy-agent --config " execStart;
in
{
  hostenv-deploy-agent-config-json =
    asserts.assertTrue "hostenv-deploy-agent-config-json"
      (hasConfigPath && configMatches && serviceStable)
      "hostenv-deploy-agent should render a config file and keep the root-owned hostenv-deploy-agent service wiring stable";
}
