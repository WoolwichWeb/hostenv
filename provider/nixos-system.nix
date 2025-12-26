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
  envUsers = builtins.attrNames (config.environments or { });
  deployPublicKey =
    if inputs ? parent
    && inputs.parent ? lib
    && inputs.parent.lib ? provider
    then inputs.parent.lib.provider.deployPublicKey or null
    else null;
  warnInvalidDeployKey =
    if inputs ? parent
    && inputs.parent ? lib
    && inputs.parent.lib ? provider
    then inputs.parent.lib.provider.warnInvalidDeployKey or true
    else true;

  hostenvEnvModule = {
    hostenv = {
      environments = config.environments or { };
      defaultEnvironment = config.defaultEnvironment or "main";
    };
  };

  userPackages = userInfo:
    let
      envOnly = packages.lib.filterAttrs (name: _: builtins.elem name envUsers) userInfo.users.users;
    in
    {
      users.users = packages.lib.concatMapAttrs
        (name: _user:
          let
            userPackage = inputs.${name}.packages.${system}.${(environmentWith name).hostenv.environmentName};
          in
          { ${name}.packages = [ userPackage ]; }
        )
        envOnly;
    };

  envUserMismatch =
    let
      envs = config.environments or { };
      mismatched =
        packages.lib.filterAttrs
          (name: env: (env.hostenv.userName or name) != name)
          envs;
    in
    builtins.attrNames mismatched;

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
      envOnly = packages.lib.filterAttrs (name: _: builtins.elem name envUsers) userInfo.users.users;
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
        envOnly;
    };

in
if envUserMismatch == [ ] then
  nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs system deployPublicKey warnInvalidDeployKey; };
    modules = [
      ./common.nix
      { sops.defaultSopsFile = secretsPath; }
      ../platform/nixos-modules/top-level.nix
    ../platform/nixos-modules/nginx-hostenv.nix
      ../platform/nixos-modules/nginx-tuning-hostenv.nix
      ../platform/nixos-modules/backups-hostenv.nix
      ../platform/nixos-modules/monitoring-hostenv.nix
      hostenvEnvModule
      (nodePath + "/configuration.nix")
      nodeConfig
      (sopsSecrets nodeConfig)
      (userPackages nodeConfig)
    ];
  }
else
  throw "hostenv provider: environment keys must match hostenv.userName (mismatched: ${builtins.toString envUserMismatch})"
