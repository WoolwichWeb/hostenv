{ ... }:
{
  flake.modules.nixos.provider-common =
    { config, lib, pkgs, ... }:
    let
      deployPublicKeys = config.provider.deployPublicKeys or [ ];
      deployUser = config.provider.deployUser or "deploy";
      trustedPublicKeys =
        if config.provider ? nixSigning && config.provider.nixSigning ? trustedPublicKeys
        then config.provider.nixSigning.trustedPublicKeys
        else [ ];
    in
    {
      options.provider = {
        deployPublicKeys = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "SSH public keys for the deploy user (provider-level).";
        };
        deployUser = lib.mkOption {
          type = lib.types.str;
          default = "deploy";
          description = "SSH/sudo user used by deploy-rs and provider tooling.";
        };
        nixSigning.trustedPublicKeys = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Public signing keys trusted by nix-daemon on provider nodes.";
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
            !include ${config.sops.secrets.access_tokens.path}
          '';

          # Allow deploy-rs uploads from the provider host without disabling
          # signature checks globally on the node.
          settings.trusted-users = lib.mkAfter [ deployUser ];
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

        # Gitlab/Github access tokens for projects are kept in this key.
        sops.secrets.access_tokens = lib.mkDefault {
          mode = "0440";
          group = config.users.groups.keys.name;
        };

        users.mutableUsers = lib.mkDefault false;
        users.users.${deployUser} = {
          isNormalUser = lib.mkDefault true;
          extraGroups = lib.mkDefault [ "wheel" "keys" ];
          openssh.authorizedKeys.keys = lib.mkDefault deployPublicKeys;
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
