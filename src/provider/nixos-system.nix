{ config
, nixpkgs
, pkgs
, node
, inputs
, localSystem
, nodesPath ? ../../nodes
, secretsPath ? ../secrets/secrets.yaml
, nodeSystems ? { }
, ...
}:
let
  nodesBase =
    if builtins.isPath nodesPath then nodesPath else builtins.path { path = nodesPath; name = "nodes"; };
  nodePath =
    if builtins.typeOf node == "path"
    then node
    else (nodesBase + "/${node}");

  system =
    if builtins.hasAttr node nodeSystems then nodeSystems.${node}
    else builtins.throw "No system specified for node '${node}' (set provider.nodeSystems.${node})";

  nodeConfig = config.nodes.${node};
  environmentWith = userName: config.environments.${userName};
  packages = pkgs.${system};
  lib = nixpkgs.lib;

  userPackages = userInfo:
    {
      users.users = packages.lib.concatMapAttrs
        (name: user:
          let
            userPackage = inputs.${name}.packages.${system}.${(environmentWith name).hostenv.environmentName};
          in
          { ${name}.packages = [ userPackage ]; }
        )
        userInfo.users.users;
    };

  sopsSecrets = userInfo:
    let
      hostPkgs = pkgs.${localSystem};
      readYaml = hostPkgs.callPackage ./read-yaml.nix { };
      secretsPathResolved =
        if builtins.pathExists secretsPath then secretsPath
        else throw "secretsPath '${builtins.toString secretsPath}' does not exist";
      sopsKeys = readYaml secretsPathResolved;
      orgFromName = name: (environmentWith name).hostenv.organisation;
      orgProjectFromName = name:
        (environmentWith name).hostenv.organisation
        + "_" + (environmentWith name).hostenv.project;
    in
    {
      sops.secrets = packages.lib.concatMapAttrs
        (
          name: _user:
            let
              name' =
                if builtins.hasAttr name sopsKeys
                then name
                else if builtins.hasAttr (orgProjectFromName name) sopsKeys
                then orgProjectFromName name
                else if builtins.hasAttr (orgFromName name) sopsKeys
                then orgFromName name
                else
                  throw ''
                    The secrets file does not contain any secrets for '${name}'

                    From the hosting root directory, run `sops secrets/secrets.yaml` and add an entry for:

                    - '${name}' (this environment) or
                    - '${orgProjectFromName name}' (this project) or
                    - '${orgFromName name}' (this organisation).
                  '';
            in
            {
              "${name}/backups_secret" = {
                owner = name;
                group = name;
                key = "${name'}/backups_secret";
              };
              "${name}/backups_env" = {
                owner = name;
                group = name;
                key = "${name'}/backups_env";
              };
            }
        )
        userInfo.users.users;
    };

in
nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = { inherit inputs system; };
  modules = [
    ./common.nix
    ../modules/nixos/top-level.nix
    ../modules/nixos/plan-bridge.nix
    ../modules/nixos/users-slices.nix
    ../modules/nixos/nginx-hostenv.nix
    ../modules/nixos/nginx-tuning-hostenv.nix
    ../modules/nixos/backups-hostenv.nix
    ../modules/nixos/monitoring-hostenv.nix
    (nodePath + /configuration.nix)
    nodeConfig
    (sopsSecrets nodeConfig)
    (userPackages nodeConfig)
  ];
}
