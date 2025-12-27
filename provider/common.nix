{ inputs, system, config, lib, ... }:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  secretFile = ../secrets/secrets.yaml;
  deployPublicKeys = config.provider.deployPublicKeys or [ ];
in
{
  options.provider = {
    deployPublicKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "SSH public keys for the deploy user (provider-level).";
    };
  };

  imports = [ inputs.sops-nix.nixosModules.sops ];

  config = {
    services.qemuGuest.enable = true;
    sops.defaultSopsFile = lib.mkDefault secretFile;
    sops.defaultSopsFormat = "yaml";
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    nix = {
      settings.auto-optimise-store = false;

      gc = {
        automatic = true;
        randomizedDelaySec = "14m";
        options = "--delete-older-than 10d";
      };

      extraOptions = ''
        experimental-features = nix-command flakes
        !include ${config.sops.secrets.access_tokens.path}
      '';

      settings.trusted-users = [ "deploy" ];
    };

    environment.systemPackages = with pkgs; [
      vim
      wget
      git
    ];

    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
      settings.KbdInteractiveAuthentication = false;
    };

    security.sudo.extraRules = [{
      groups = [ "wheel" ];
      commands = [{
        command = "ALL";
        options = [ "NOPASSWD" ];
      }];
    }];

    users.groups.keys.name = "keys";

    sops.secrets.access_tokens = {
      mode = "0440";
      group = config.users.groups.keys.name;
    };

    users.mutableUsers = false;
    users.users.deploy = {
      isNormalUser = true;
      extraGroups = [ "wheel" "keys" ];
      openssh.authorizedKeys.keys = lib.mkDefault deployPublicKeys;
    };

    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [ 80 443 22 ];

    environment.sessionVariables = {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
    };
  };
}
