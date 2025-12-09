{ inputs, system, config, ... }:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  secretFile =
    if builtins.pathExists ../secrets/secrets.yaml
    then ../secrets/secrets.yaml
    else ../secrets/secrets.yaml.example;
in
{

  services.qemuGuest.enable = true;

  imports = [ inputs.sops-nix.nixosModules.sops ];
  sops.defaultSopsFile = secretFile;
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
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ7jiIqEDu1TAI2OL8cI575ufkhPJ1fxqC6qmJPaj5s0 liam"
    ];
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 80 443 22 ];

  environment.sessionVariables = {
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

}
