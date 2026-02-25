{ ... }:
{
  flake.modules.nixos.provider-common =
    { config, lib, pkgs, ... }:
    let
      cominCfg = config.provider.comin;
      serviceCfg = config.provider.service;
      trustedPublicKeys = config.provider.nixSigning.trustedPublicKeys;
      cominActivateScript = pkgs.writeShellApplication {
        name = "hostenv-comin-activate";
        runtimeInputs = [
          pkgs.coreutils
          pkgs.curl
          pkgs.gnused
          pkgs.gnugrep
          pkgs.jq
          pkgs.systemd
          pkgs.util-linux
        ];
        text = builtins.readFile ./hostenv-comin-activate.sh;
      };
      cominActivateWrapper = pkgs.writeShellScript "hostenv-comin-activate-wrapper" ''
        export HOSTENV_COMIN_NODE_NAME=${cominCfg.nodeName}
        export HOSTENV_COMIN_API_BASE_URL=${cominCfg.providerApiBaseUrl}
        export HOSTENV_COMIN_TOKEN_FILE=${cominCfg.nodeAuthTokenFile}
        export HOSTENV_COMIN_ACTION_TIMEOUT=${toString cominCfg.actionTimeoutSeconds}
        exec ${cominActivateScript}/bin/hostenv-comin-activate
      '';

    in
    {
      options.provider = {
        nixSigning.trustedPublicKeys = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Public signing keys trusted by nix-daemon on provider nodes.";
        };
        service = lib.mkOption {
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              organisation = lib.mkOption {
                type = lib.types.str;
                description = "Organisation that owns the provider-service environment.";
              };
              project = lib.mkOption {
                type = lib.types.str;
                description = "Project that owns the provider-service environment.";
              };
              environmentName = lib.mkOption {
                type = lib.types.str;
                description = "Environment name that runs the provider service and receives provider secrets.";
              };
            };
          });
          default = null;
          description = "Provider environment selector used for provider-service secrets.";
        };
        comin = {
          enable = lib.mkEnableOption "pull-based node reconciliation using comin";
          remoteUrl = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Git URL for the provider repository consumed by comin.";
          };
          branch = lib.mkOption {
            type = lib.types.str;
            default = "main";
            description = "Branch comin should poll for desired-state updates.";
          };
          pollIntervalSeconds = lib.mkOption {
            type = lib.types.int;
            default = 30;
            description = "Polling interval (seconds) for comin remotes.";
          };
          actionTimeoutSeconds = lib.mkOption {
            type = lib.types.int;
            default = 900;
            description = "Maximum seconds a single comin user action may run before timing out.";
          };
          providerApiBaseUrl = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Base URL for provider callback APIs.";
          };
          nodeAuthTokenFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Bearer token file used by node callback requests.";
          };
          nodeName = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Logical node identity used for deploy intent lookup.";
          };
        };
      };

      config = {
        sops.defaultSopsFormat = lib.mkDefault "yaml";
        sops.age.sshKeyPaths = lib.mkDefault [ "/etc/ssh/ssh_host_ed25519_key" ];

        nix = {
          # Currently disabled `nix.settings.auto-optimise-store` as it seems to fail with remote builders
          # TODO re-enable when fixed https://github.com/NixOS/nix/issues/7273
          settings.auto-optimise-store = lib.mkDefault false;

          gc = {
            automatic = lib.mkDefault true;
            randomizedDelaySec = lib.mkDefault "14m";
            options = lib.mkDefault "--delete-older-than 10d";
          };

          extraOptions = lib.mkDefault ''
            experimental-features = nix-command flakes
          '';

          settings.trusted-public-keys = lib.mkAfter trustedPublicKeys;
        };

        services.openssh = {
          enable = lib.mkDefault true;
          settings.PasswordAuthentication = lib.mkDefault false;
          settings.KbdInteractiveAuthentication = lib.mkDefault false;
        };

        security.sudo.extraRules = [{
          groups = [ "wheel" ];
          commands = [{
            command = "ALL";
            options = [ "NOPASSWD" ];
          }];
        }];

        users.groups.keys.name = lib.mkDefault "keys";

        users.mutableUsers = lib.mkDefault false;

        assertions = [
          {
            assertion = (!cominCfg.enable) || (cominCfg.remoteUrl != null && cominCfg.remoteUrl != "");
            message = "provider.comin.remoteUrl must be configured when provider.comin.enable is true.";
          }
          {
            assertion = (!cominCfg.enable) || (cominCfg.providerApiBaseUrl != null && cominCfg.providerApiBaseUrl != "");
            message = "provider.comin.providerApiBaseUrl must be configured when provider.comin.enable is true.";
          }
          {
            assertion = (!cominCfg.enable) || (cominCfg.nodeAuthTokenFile != null && cominCfg.nodeAuthTokenFile != "");
            message = "provider.comin.nodeAuthTokenFile must be configured when provider.comin.enable is true.";
          }
          {
            assertion = (!cominCfg.enable) || (cominCfg.nodeName != null && cominCfg.nodeName != "");
            message = "provider.comin.nodeName must be configured when provider.comin.enable is true.";
          }
          {
            assertion =
              (!cominCfg.enable)
              || (serviceCfg != null
                && serviceCfg.organisation != ""
                && serviceCfg.project != ""
                && serviceCfg.environmentName != "");
            message = "provider.service.organisation/project/environmentName must be configured when provider.comin.enable is true.";
          }
          {
            assertion = cominCfg.pollIntervalSeconds > 0;
            message = "provider.comin.pollIntervalSeconds must be greater than zero.";
          }
          {
            assertion = cominCfg.actionTimeoutSeconds > 0;
            message = "provider.comin.actionTimeoutSeconds must be greater than zero.";
          }
        ];

        sops.secrets = lib.mkIf cominCfg.enable {
          hostenv-comin-node-token = {
            key = "comin_node_tokens/${cominCfg.nodeName}";
            path = cominCfg.nodeAuthTokenFile;
            owner = "root";
            group = "root";
            mode = "0400";
          };
        };

        services.comin = lib.mkIf cominCfg.enable {
          enable = true;
          hostname = cominCfg.nodeName;
          remotes = [
            {
              name = "hostenv";
              url = cominCfg.remoteUrl;
              branches = [ cominCfg.branch ];
              poller.period = cominCfg.pollIntervalSeconds;
            }
          ];
          postDeploymentCommand = "${cominActivateWrapper}";
        };

        networking.firewall.enable = lib.mkDefault true;
        # Do not use mkDefault here: list options only merge within the
        # strongest priority class, and sshd's non-default openFirewall rule
        # would otherwise collapse this list down to [ 22 ].
        networking.firewall.allowedTCPPorts = [ 80 443 22 ];

        environment.sessionVariables = {
          XDG_CACHE_HOME = lib.mkDefault "$HOME/.cache";
          XDG_CONFIG_HOME = lib.mkDefault "$HOME/.config";
          XDG_DATA_HOME = lib.mkDefault "$HOME/.local/share";
          XDG_STATE_HOME = lib.mkDefault "$HOME/.local/state";
        };
      };
    }
  ;
}
