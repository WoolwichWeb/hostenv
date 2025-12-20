{ lib, pkgs, config, ... }:
let
  allEnvs = config.hostenv.environments or { };
  # Treat enable = false as disabled; default to enabled when unset.
  envs = lib.filterAttrs (_: env: env.enable != false) allEnvs;
  mkUpstream = envName: env:
    let
      user = env.hostenv.userName or envName;
      socket = (env.hostenv.upstreamRuntimeDir or "/run/hostenv/nginx/${user}") + "/in.sock";
    in {
      servers = { "unix:${socket}" = { }; };
    };
  vhostFromEnv = name: env:
    let
      primary = env.hostenv.hostname or name;
      baseVH = env.virtualHosts or { };
      enableACMEDefault = config.hostenv.nginx.enableACME;
      defaultLoc = {
        "/" = {
          recommendedProxySettings = true;
          proxyPass = "http://${name}_upstream";
        };
      };
      addPrimary = baseVH // {
        ${primary} = (baseVH.${primary} or { }) // {
          enableACME = lib.mkDefault (baseVH.${primary}.enableACME or baseVH.${primary}.enableLetsEncrypt or enableACMEDefault);
          locations = (baseVH.${primary}.locations or { }) // defaultLoc;
          forceSSL = lib.mkDefault (baseVH.${primary}.forceSSL or baseVH.${primary}.enableACME or baseVH.${primary}.enableLetsEncrypt or enableACMEDefault);
          http2 = true;
          hsts = baseVH.${primary}.hsts or true;
          # Simple header set (fixed defaults).
          extraHeaders = {
            "Strict-Transport-Security" = "max-age=63072000; includeSubDomains; preload";
            "X-Frame-Options" = "SAMEORIGIN";
            "X-Content-Type-Options" = "nosniff";
          };
        };
      };
    in addPrimary;
in
{
  options.hostenv.nginx.enableACME = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Whether host-level nginx should request ACME certs by default.";
  };

  config = lib.mkIf (envs != { }) {
    assertions = lib.flatten (lib.mapAttrsToList (n: env: [
      {
        assertion = (env.hostenv.upstreamRuntimeDir or "") != "";
        message = "hostenv nginx: upstreamRuntimeDir missing for env ${n}";
      }
    ]) envs);

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = lib.foldl' lib.recursiveUpdate { }
        (lib.mapAttrsToList (n: env: vhostFromEnv (env.hostenv.userName or n) env) envs);
      upstreams = lib.listToAttrs (lib.mapAttrsToList
        (n: env:
          let user = env.hostenv.userName or n;
          in {
            name = "${user}_upstream";
            value = mkUpstream n env;
          })
        envs);
    };
  };
}
