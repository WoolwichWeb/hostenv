{ inputs
, config
, nixpkgs
, deploy-rs
, systems
, localSystem
, nodesPath
, secretsPath
, nodeSystems ? { }
}:
let
  forEachSystem = nixpkgs.lib.genAttrs (import systems);
  pkgs = forEachSystem (system: import nixpkgs { inherit system; });

  nixosSystem = node: import ./nixos-system.nix {
    inherit config node nixpkgs pkgs inputs localSystem nodesPath secretsPath nodeSystems;
  };

  nodes = builtins.mapAttrs
    (node: _: nixosSystem node)
    config.nodes;

  environments = forEachSystem (system: builtins.mapAttrs
    (name: environment: pkgs.${system}.buildEnv {
      inherit name;
      paths = [ inputs.${name}.packages.${system}.${environment.hostenv.environmentName} ];
    })
    config.environments
  );

  environmentsWith = node: forEachSystem (system: (
    nixpkgs.lib.filterAttrs
      (name: environment: node == config.environments.${name}.node)
      environments.${system}
  ));
in
{
  inherit inputs config environmentsWith;

  packages = {
    inherit nodes environments;
  };

  deploy.nodes = builtins.mapAttrs
    (node: _: {
      hostname = node + "." + config.hostenvHostname;
      fastConnection = true;
      remoteBuild = true;
      profilesOrder = [ "system" ] ++ builtins.attrNames (environmentsWith node).${localSystem};
      profiles =
        let
          remoteSystem = nodes.${node}.config.nixpkgs.hostPlatform.system;
        in
        {
          system = {
            sshUser = "deploy";
            user = "root";
            path = deploy-rs.lib.${remoteSystem}.activate.nixos nodes.${node};
          };
        } // builtins.mapAttrs
          (name: environment: {
            user = name;
            sshUser = name;
            path = deploy-rs.lib.${remoteSystem}.activate.custom environment "./bin/activate";
          })
          (environmentsWith node).${remoteSystem};
      checks = { };
    })
    config.nodes;

  nixosConfigurations = nodes;
}
