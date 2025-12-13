# Bridge provider-evaluated environments (config.environments) into hostenv.environments
# so host-level modules can operate without provider-specific glue.
{ lib, config, ... }:
let
  envs = config.environments or { };
  defaultEnv = config.defaultEnvironment or "main";
in
{
  config.hostenv.environments = lib.mapAttrs
    (envName: envCfg:
      let
        vh = envCfg.virtualHosts or { };
        primaryHost = envCfg.hostenv.hostname or envName;
        userName = envCfg.hostenv.userName or envName;
        allKeys = lib.foldlAttrs (acc: _n: u: acc ++ (u.publicKeys or [ ])) [ ] (envCfg.users or { });
      in {
        enable = envCfg.enable or true;
        type = envCfg.type or "development";
        user = userName;
        hostname = primaryHost;
        runtimeDir = envCfg.hostenv.runtimeDir or "/run/hostenv/user/${userName}";
        upstreamRuntimeDir = envCfg.hostenv.upstreamRuntimeDir or "/run/hostenv/nginx/${userName}";
        virtualHosts = vh;
        phpVersion = envCfg.hostenv.phpVersion or null;
        dbName = envCfg.hostenv.dbName or null;
        extras =
          (envCfg.hostenv.extras or { })
          // {
            uid = envCfg.uid or null;
            publicKeys = allKeys;
          };
      })
    envs;

  config.hostenv.defaultEnvironment = defaultEnv;
}
