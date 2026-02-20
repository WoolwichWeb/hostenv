{ pkgs, inputs }:
let
  system = pkgs.stdenv.hostPlatform.system;
  envName = "acme__demo-main";
  hostName = "${envName}.hostenv.test";
  nodeName = "node-a";
  deployUser = "shipper";
  trustedSigningKey = "hostenv-provider-test-1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  ageRecipient = "age1s4c2vcrfg4sx6knkmgyayfyvhr3plyehsaut9qpp5n9zhln56sqs8c40ap";
  ageSecretKey = "AGE-SECRET-KEY-1T98SVZ66YW95TD5JEVXAA9NGY06EZU397XXWGXA79S584NXVU8ASGQMDJD";

  nodesPath = pkgs.runCommand "provider-secrets-runtime-nodes-stub" { } ''
    mkdir -p "$out/${nodeName}"
    cat > "$out/${nodeName}/configuration.nix" <<'EOF'
    { ... }: {
      system.stateVersion = "24.11";
      networking.hostName = "node-a";
    }
    EOF
  '';

  secretsFixture = pkgs.runCommand "provider-secrets-runtime-secrets" { nativeBuildInputs = [ pkgs.sops ]; } ''
    set -euo pipefail
    mkdir -p "$out"

    cat > "$out/plain.yaml" <<'EOF'
    acme__demo-main:
      backups_secret: "runtime-backups-secret"
      backups_env: "RESTIC_PASSWORD=runtime-backups-env"
      env_only: "runtime-env-only"
    acme_demo:
      project_only: "runtime-project-only"
    EOF

    ${pkgs.sops}/bin/sops \
      --encrypt \
      --input-type yaml \
      --output-type yaml \
      --age "${ageRecipient}" \
      "$out/plain.yaml" \
      > "$out/secrets.yaml"
  '';

  secretsPath = secretsFixture + "/secrets.yaml";

  envInput = {
    packages.${system}.main = pkgs.hello;
  };

  nodeSystems = { "${nodeName}" = system; };

  config = {
    deployUser = deployUser;
    nixSigning.trustedPublicKeys = [ trustedSigningKey ];
    nodes = {
      "${nodeName}" = {
        users.users = {
          "${envName}" = { };
        };
        provider = {
          deployPublicKeys = [ "ssh-ed25519 test" ];
          deployUser = deployUser;
          nixSigning.trustedPublicKeys = [ trustedSigningKey ];
        };
      };
    };
    environments = {
      "${envName}" = {
        enable = true;
        type = "production";
        uid = 1001;
        node = nodeName;
        secrets = {
          enable = true;
          keys = [ "env_only" ];
        };
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "main";
          userName = envName;
          hostname = hostName;
          hostenvHostname = "hosting.test";
          upstreamRuntimeDir = "/run/hostenv/nginx/${envName}";
          root = "/src/demo";
          projectSecrets = {
            enable = true;
            keys = [ "project_only" ];
          };
        };
      };
    };
    defaultEnvironment = "main";
  };

  providerFlake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports =
      let
        modules = inputs.import-tree ../../modules;
        moduleList = if builtins.isList modules then modules else [ modules ];
      in
      [ inputs.devshell.flakeModule ] ++ moduleList;
    provider.enable = true;
    project.enable = false;
    provider = {
      hostenvHostname = "hosting.test";
      deployPublicKeys = [ "ssh-ed25519 test" ];
      deployUser = deployUser;
      nixSigning.trustedPublicKeys = [ trustedSigningKey ];
      nodeFor = { default = nodeName; };
      nodeSystems = nodeSystems;
    };
  };

  pkgsBySystem = pkgs.lib.genAttrs [ system ] (_: pkgs);
  inputsForSystem = inputs // {
    "${envName}" = envInput;
    parent = providerFlake;
  };

  systemEval = providerFlake.lib.provider.nixosSystem {
    inherit config nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

in
pkgs.testers.runNixOSTest ({ ... }: {
  name = "provider-secrets-runtime";

  nodes.machine = {
    imports = systemEval._module.args.modules ++ [
      ({ ... }: {
        sops.age.keyFile = "/etc/sops/age/keys.txt";
        environment.etc."sops/age/keys.txt".text = "${ageSecretKey}\n";
        environment.etc."sops/age/keys.txt".mode = "0400";
        security.acme.acceptTerms = true;
        security.acme.defaults.email = "test@example.invalid";
        virtualisation.memorySize = 2048;
      })
    ];
  };

  testScript = ''
    machine.wait_for_unit("multi-user.target")
    machine.wait_until_succeeds("test -f /run/secrets/${envName}/project_only", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/${envName}/env_only", timeout=180)

    machine.succeed("test \"$(cat /run/secrets/${envName}/project_only)\" = runtime-project-only")
    machine.succeed("test \"$(cat /run/secrets/${envName}/env_only)\" = runtime-env-only")

    machine.succeed("test \"$(stat -Lc '%U:%G' /run/secrets/${envName}/project_only)\" = ${envName}:${envName}")
    machine.succeed("test \"$(stat -Lc '%U:%G' /run/secrets/${envName}/env_only)\" = ${envName}:${envName}")

    machine.succeed("test \"$(stat -Lc '%a' /run/secrets/${envName}/project_only)\" = 400")
    machine.succeed("test \"$(stat -Lc '%a' /run/secrets/${envName}/env_only)\" = 400")

    machine.succeed("test ! -e /run/secrets/deploy/project_only")
  '';
})
