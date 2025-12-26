{ pkgs, inputs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };

  system = pkgs.stdenv.hostPlatform.system;
  envName = "acme__demo-main";
  hostName = "${envName}.hostenv.test";
  nodeName = "node-a";

  nodesPath = pkgs.runCommand "nodes-stub" { } ''
    mkdir -p "$out/${nodeName}"
    cat > "$out/${nodeName}/configuration.nix" <<'EOF'
    { ... }: {
      system.stateVersion = "24.11";
      networking.hostName = "node-a";
    }
    EOF
  '';

  secretsPath = pkgs.writeText "secrets.yaml" ''
    ${envName}:
      backups_secret: "dummy"
      backups_env: "dummy"
  '';

  envInput = {
    packages.${system}.main = pkgs.hello;
  };

  inputsForSystem = inputs // { "${envName}" = envInput; };
  nodeSystems = { "${nodeName}" = system; };

  config = {
    nodes = {
      "${nodeName}" = {
        users.users = {
          "${envName}" = { };
          deploy = { };
        };
      };
    };
    environments = {
      "${envName}" = {
        enable = true;
        type = "production";
        uid = 1001;
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "main";
          userName = envName;
          hostname = hostName;
          hostenvHostname = "hosting.test";
          upstreamRuntimeDir = "/run/hostenv/nginx/${envName}";
          root = "/src/demo";
        };
      };
    };
    defaultEnvironment = "main";
  };

  configMismatch = config // {
    environments = config.environments // {
      "${envName}" = (config.environments.${envName} // {
        hostenv = (config.environments.${envName}.hostenv // { userName = "wrong-user"; });
      });
    };
  };

  providerFlake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports = [ ../../provider/flake-module.nix ];
    provider = {
      hostenvHostname = "hosting.test";
      deployPublicKey = "ssh-ed25519 test";
      nodeFor = { default = nodeName; };
      nodeSystems = nodeSystems;
    };
  };

  pkgsBySystem = lib.genAttrs [ system ] (_: pkgs);

  nixosSystemPath = providerFlake.lib.provider.nixosSystem;
  systemEval = import nixosSystemPath {
    inherit config nodeSystems nodeName nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

  systemMismatch = builtins.tryEval (import nixosSystemPath {
    config = configMismatch;
    inherit nodeSystems nodeName nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  });

  nginxOk = systemEval.config.services.nginx.enable == true;
  vhostOk = builtins.hasAttr hostName systemEval.config.services.nginx.virtualHosts;
in
asserts.assertTrue "provider-nixos-system-eval"
  (nginxOk && vhostOk && ! systemMismatch.success)
  "provider nixosSystem should enforce env key/userName alignment"
