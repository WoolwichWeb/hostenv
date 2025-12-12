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

  tmpfilesRules = userInfo: {
    systemd.tmpfiles.rules = [
      "d    /run/hostenv           0755  root   root    -"
      "d    /run/hostenv/nginx     0755  root   root    -"
      "d    /run/hostenv/user      0755  root   root    -"
    ]
    ++ packages.lib.flatten (packages.lib.mapAttrsToList
      (name: _user: [
        "d /run/hostenv/nginx/${name}  2770  ${name} nginx -"
        "d /run/hostenv/user/${name}   2700  ${name} users -"
      ])
      userInfo.users.users);
  };

  nginxUpstreams = userInfo: packages.lib.listToAttrs (packages.lib.mapAttrsToList
    (name: _user: {
      name = "${name}_upstream";
      value = {
        servers = {
          "unix:/run/hostenv/nginx/${name}/in.sock" = { };
        };
      };
    })
    userInfo.users.users);

  commonHttpConfig = ''
    log_format upstream_params '[$time_local] $remote_addr - $server_port - $remote_user - $host - $proxy_add_x_forwarded_for - $scheme to: $upstream_addr ($request $status $upstream_response_time $upstream_response_time msec $msec request_time $request_time)';
    access_log syslog:server=unix:/dev/log upstream_params;
  '';

  clientMaxBodySize = "2G";
  proxyTimeout = "600s";

  appendHttpConfig = ''
    proxy_buffer_size   128k;
    proxy_buffers   4 256k;
    proxy_busy_buffers_size   256k;
  '';

  nginx = {
    services.nginx = {
      enable = true;
      recommendedProxySettings = true;

      inherit commonHttpConfig clientMaxBodySize proxyTimeout appendHttpConfig;

      upstreams = nginxUpstreams nodeConfig;
    };
  };
in
nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = { inherit inputs system; };
  modules = [
    ./common.nix
    ../modules/hostenv.nix
    ../modules/nixos/users-slices.nix
    ../modules/nixos/nginx-hostenv.nix
    ../modules/nixos/backups-hostenv.nix
    ../modules/nixos/monitoring-hostenv.nix
    # Bridge plan.json → hostenv.environments so dendritic feature modules can consume it.
    ({ ... }: {
      hostenv.environments = builtins.mapAttrs
        (envName: envCfg:
          let
            vh = envCfg.virtualHosts or { };
            primaryHost = envCfg.hostenv.hostname or envName;
            allKeys = lib.foldlAttrs (acc: _n: u: acc ++ (u.publicKeys or [])) [] (envCfg.users or {});
          in {
            enable = envCfg.enable or true;
            type = envCfg.type or "development";
            user = envCfg.hostenv.userName or envName;
            hostname = primaryHost;
            runtimeDir = envCfg.hostenv.runtimeDir or "/run/hostenv/user/${envCfg.hostenv.userName or envName}";
            upstreamRuntimeDir = envCfg.hostenv.upstreamRuntimeDir or "/run/hostenv/nginx/${envCfg.hostenv.userName or envName}";
            virtualHosts = vh;
            phpVersion = envCfg.hostenv.phpVersion or null;
            dbName = envCfg.hostenv.dbName or null;
            extras = {
              uid = envCfg.uid or null;
              publicKeys = allKeys;
            };
          })
        config.environments;
      hostenv.defaultEnvironment = config.defaultEnvironment or "main";
    })
    (nodePath + /configuration.nix)
    nodeConfig
    (tmpfilesRules nodeConfig)
    (sopsSecrets nodeConfig)
    (userPackages nodeConfig)
    nginx
  ];
}
