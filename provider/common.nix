{ inputs, system, config, lib, deployPublicKey ? null, warnInvalidDeployKey ? true, ... }:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  secretFile = ../secrets/secrets.yaml;
  deployPublicKeyList =
    let
      key = deployPublicKey;
      parts =
        if key == null
        then [ ]
        else lib.filter (s: s != "") (lib.strings.splitString " " key);
      keyType = if builtins.length parts > 0 then builtins.elemAt parts 0 else "";
      keyData = if builtins.length parts > 1 then builtins.elemAt parts 1 else "";
      allowedKeyTypes = [
        "ssh-ed25519"
        "ssh-rsa"
        "ecdsa-sha2-nistp256"
        "ecdsa-sha2-nistp384"
        "ecdsa-sha2-nistp521"
        "sk-ssh-ed25519@openssh.com"
        "sk-ecdsa-sha2-nistp256@openssh.com"
      ];
      isValid =
        key != null
        && key != ""
        && builtins.length parts >= 2
        && lib.elem keyType allowedKeyTypes
        && builtins.match "^[A-Za-z0-9+/=]+$" keyData != null;
    in
    if key == null || key == "" then
      if warnInvalidDeployKey then
        lib.warn "provider.deployPublicKey is unset or empty; skipping deploy key for provider user." [ ]
      else
        [ ]
    else if isValid then
      [ key ]
    else if warnInvalidDeployKey then
      lib.warn "provider.deployPublicKey is not a valid SSH public key; skipping it." [ ]
    else
      [ ];
in
{

  services.qemuGuest.enable = true;

  imports = [ inputs.sops-nix.nixosModules.sops ];
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
    openssh.authorizedKeys.keys = lib.mkDefault deployPublicKeyList;
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
