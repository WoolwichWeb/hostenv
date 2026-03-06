{ ... }:
{
  flake.modules.nixos.provider-common =
    { config, lib, ... }:
    let
      serviceCfg = config.provider.service;
      deployCfg = config.provider.deploy;
      cacheCfg = config.provider.cache;
      providerTrustedKeys = config.provider.nixSigning.trustedPublicKeys;
      cacheUrl =
        if cacheCfg.url != null && cacheCfg.url != ""
        then cacheCfg.url
        else null;
      cachePublicKey =
        if cacheCfg.publicKey != null && cacheCfg.publicKey != ""
        then cacheCfg.publicKey
        else null;
      trustedPublicKeys =
        lib.unique (providerTrustedKeys ++ lib.optional (cachePublicKey != null) cachePublicKey);
      enabledEnvironments = lib.filterAttrs (_: env: env.enable or true) (config.hostenv.environments or { });
      selectedServiceEnv =
        if serviceCfg == null then
          null
        else
          let
            matches = lib.filterAttrs
              (_: env:
                env.hostenv.organisation == serviceCfg.organisation
                && env.hostenv.project == serviceCfg.project
                && env.hostenv.environmentName == serviceCfg.environmentName)
              enabledEnvironments;
            values = builtins.attrValues matches;
          in
          if values == [ ] then null else builtins.head values;
      selectedServiceUser =
        if selectedServiceEnv == null then
          null
        else
          selectedServiceEnv.hostenv.userName;
      onProviderServiceNode = selectedServiceUser != null;
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

        deploy = {
          enable = lib.mkEnableOption "provider-deploy node agent";

          providerApiBaseUrl = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Base URL for provider deploy APIs.";
          };

          nodeAuthTokenFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Bearer token file used by provider-deploy.";
          };

          nodeName = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Logical node identity used by provider-deploy.";
          };

          reconnectSeconds = lib.mkOption {
            type = lib.types.int;
            default = 5;
            description = "Backoff delay between provider-deploy reconnects.";
          };
        };

        cache = {
          enable = lib.mkEnableOption "provider binary cache client configuration";

          url = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Provider cache URL used as highest-priority substituter.";
          };

          publicKey = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Public cache signing key appended to trusted-public-keys.";
          };

          netrcFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Optional netrc file path used for cache authentication.";
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
          settings.require-signed-binaries = lib.mkIf cacheCfg.enable true;
          settings.substituters = lib.mkIf (cacheCfg.enable && cacheUrl != null) (lib.mkBefore [ cacheUrl "https://cache.nixos.org" ]);
          settings.netrc-file = lib.mkIf (cacheCfg.enable && cacheCfg.netrcFile != null && cacheCfg.netrcFile != "") cacheCfg.netrcFile;
          settings.allowed-users = lib.mkIf onProviderServiceNode (lib.mkAfter [ selectedServiceUser ]);
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
            assertion = (!deployCfg.enable) || (deployCfg.providerApiBaseUrl != null && deployCfg.providerApiBaseUrl != "");
            message = "provider.deploy.providerApiBaseUrl must be configured when provider.deploy.enable is true.";
          }
          {
            assertion = (!deployCfg.enable) || (deployCfg.nodeAuthTokenFile != null && deployCfg.nodeAuthTokenFile != "");
            message = "provider.deploy.nodeAuthTokenFile must be configured when provider.deploy.enable is true.";
          }
          {
            assertion = (!deployCfg.enable) || (deployCfg.nodeName != null && deployCfg.nodeName != "");
            message = "provider.deploy.nodeName must be configured when provider.deploy.enable is true.";
          }
          {
            assertion = deployCfg.reconnectSeconds > 0;
            message = "provider.deploy.reconnectSeconds must be greater than zero.";
          }
          {
            assertion = (!cacheCfg.enable) || (cacheCfg.url != null && cacheCfg.url != "");
            message = "provider.cache.url must be configured when provider.cache.enable is true.";
          }
          {
            assertion = (!cacheCfg.enable) || (cacheCfg.publicKey != null && cacheCfg.publicKey != "");
            message = "provider.cache.publicKey must be configured when provider.cache.enable is true.";
          }
          {
            assertion =
              (!onProviderServiceNode)
              || (serviceCfg != null
                && serviceCfg.organisation != ""
                && serviceCfg.project != ""
                && serviceCfg.environmentName != "");
            message = "provider.service.organisation/project/environmentName must be configured for provider-service node detection.";
          }
        ];

        sops.secrets = lib.mkMerge [
          (lib.mkIf deployCfg.enable {
            hostenv-provider-node-token = {
              key = "provider_node_tokens/${deployCfg.nodeName}";
              path = deployCfg.nodeAuthTokenFile;
              owner = "root";
              group = "root";
              mode = "0400";
            };
          })
          (lib.mkIf (cacheCfg.enable && cacheCfg.netrcFile != null && cacheCfg.netrcFile != "") {
            hostenv-provider-cache-netrc = {
              key = "cache_netrc";
              path = cacheCfg.netrcFile;
              owner = "root";
              group = "root";
              mode = "0400";
            };
          })
        ];

        services.provider-deploy = lib.mkIf deployCfg.enable {
          enable = true;
          providerApiBaseUrl = deployCfg.providerApiBaseUrl;
          nodeAuthTokenFile = deployCfg.nodeAuthTokenFile;
          nodeName = deployCfg.nodeName;
          reconnectSeconds = deployCfg.reconnectSeconds;
        };

        users.users = lib.mkIf onProviderServiceNode {
          ${selectedServiceUser} = {
            extraGroups = [ "nixbld" ];
          };
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
