{ ... }:
{
  flake.modules.nixos.provider-common =
    { config, lib, pkgs, ... }:
    let
      deployPublicKeys = config.provider.deployPublicKeys or [ ];
    in
    {
      options.provider.deployPublicKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "SSH public keys for the deploy user (provider-level).";
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

          # See: https://discourse.nixos.org/t/build-failed-due-to-lack-of-signature-by-trusted-key-in-distributed-build/44996/2
          settings.trusted-users = lib.mkDefault [ "deploy" ];
        };

        services.openssh = {
          enable = lib.mkDefault true;
          settings.PasswordAuthentication = lib.mkDefault false;
          settings.KbdInteractiveAuthentication = lib.mkDefault false;
        };

        security.sudo.extraRules = lib.mkDefault [{
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
        users.users.deploy = {
          isNormalUser = lib.mkDefault true;
          extraGroups = lib.mkDefault [ "wheel" "keys" ];
          openssh.authorizedKeys.keys = lib.mkDefault deployPublicKeys;
        };

        networking.firewall.enable = lib.mkDefault true;
        networking.firewall.allowedTCPPorts = lib.mkDefault [ 80 443 22 ];

        environment.sessionVariables = lib.mkDefault {
          XDG_CACHE_HOME = "$HOME/.cache";
          XDG_CONFIG_HOME = "$HOME/.config";
          XDG_DATA_HOME = "$HOME/.local/share";
          XDG_STATE_HOME = "$HOME/.local/state";
        };
      };
    }
  ;
}
