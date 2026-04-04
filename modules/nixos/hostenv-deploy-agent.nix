{ ... }:
let
  hostenvDeployAgent = (import ../services/hostenv-deploy-agent/module.nix { }).config.flake.lib.provider.deployAgent;
in
{
  flake.modules.nixos."hostenv-deploy-agent" =
    { config, lib, pkgs, ... }:
    let
      cfg = config.services.hostenv-deploy-agent;
      hostenvDeployAgentRuntimeInputs = with pkgs; [
        bash
        coreutils
        nix
        shadow
        systemd
        util-linux
      ];
      compiledAgent = pkgs.haskell.packages.ghc912.callCabal2nix "hostenv-deploy-agent" hostenvDeployAgent.src { };
      agentPackage = pkgs.symlinkJoin {
        name = "hostenv-deploy-agent";
        paths = [ compiledAgent ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/hostenv-deploy-agent" \
            --prefix PATH : ${lib.makeBinPath hostenvDeployAgentRuntimeInputs}
        '';
      };
      agentConfigFile = pkgs.writers.writeJSON "hostenv-deploy-agent-config.json" {
        providerApiBaseUrl = cfg.providerApiBaseUrl;
        nodeAuthTokenFile = cfg.nodeAuthTokenFile;
        nodeName = cfg.nodeName;
        stateFile = cfg.stateFile;
        actionTimeoutSeconds = cfg.actionTimeoutSeconds;
        reconnectSeconds = cfg.reconnectSeconds;
      };
    in
    {
      options.services.hostenv-deploy-agent = {
        enable = lib.mkEnableOption "hostenv provider deploy node agent";

        providerApiBaseUrl = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "Base URL for provider deploy APIs.";
        };

        nodeAuthTokenFile = lib.mkOption {
          type = lib.types.str;
          default = "/run/secrets/hostenv/provider_node_token";
          description = "Path to bearer token used by hostenv-deploy-agent.";
        };

        nodeName = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "Logical node name used by hostenv-deploy-agent.";
        };

        reconnectSeconds = lib.mkOption {
          type = lib.types.int;
          default = 5;
          description = "Reconnect/backoff delay in seconds.";
        };

        stateFile = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/hostenv-deploy-agent/state.json";
          description = "Local hostenv-deploy-agent state file path.";
        };

        actionTimeoutSeconds = lib.mkOption {
          type = lib.types.int;
          default = 1800;
          description = "Maximum seconds allowed per deploy action.";
        };

        watchdogSec = lib.mkOption {
          type = lib.types.nullOr lib.types.int;
          default = null;
          description = "Optional systemd watchdog interval for hostenv-deploy-agent.";
        };
      };

      config = lib.mkIf cfg.enable {
        security.pam.services.runuser.setEnvironment = lib.mkForce true;

        assertions = [
          {
            assertion = cfg.providerApiBaseUrl != "";
            message = "services.hostenv-deploy-agent.providerApiBaseUrl must be set when services.hostenv-deploy-agent.enable is true.";
          }
          {
            assertion = cfg.nodeName != "";
            message = "services.hostenv-deploy-agent.nodeName must be set when services.hostenv-deploy-agent.enable is true.";
          }
          {
            assertion = cfg.reconnectSeconds > 0;
            message = "services.hostenv-deploy-agent.reconnectSeconds must be greater than zero.";
          }
          {
            assertion = cfg.actionTimeoutSeconds > 0;
            message = "services.hostenv-deploy-agent.actionTimeoutSeconds must be greater than zero.";
          }
          {
            assertion = cfg.watchdogSec == null || cfg.watchdogSec > 0;
            message = "services.hostenv-deploy-agent.watchdogSec must be null or greater than zero.";
          }
        ];

        systemd.services.hostenv-deploy-agent = {
          description = "Hostenv provider deploy node agent";
          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          serviceConfig = {
            Type = "notify";
            NotifyAccess = "all";
            ExecStart = "${agentPackage}/bin/hostenv-deploy-agent --config ${agentConfigFile}";
            Restart = "always";
            RestartSec = "5s";
            TimeoutStartSec = "0";
            DynamicUser = false;
            User = "root";
            StateDirectory = "hostenv-deploy-agent";
          } // lib.optionalAttrs (cfg.watchdogSec != null) {
            WatchdogSec = "${toString cfg.watchdogSec}s";
          };
        };
      };
    }
  ;
}
