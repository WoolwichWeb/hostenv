{ lib, pkgs, config, ... }:
let
  allEnvs = config.hostenv.environments or { };
  envs = lib.filterAttrs (_: env: env.enable or true) allEnvs;
  mkUpstream = envName: env:
    let
      user = env.user or envName;
      socket = (env.upstreamRuntimeDir or "/run/hostenv/nginx/${user}") + "/in.sock";
    in {
      servers = { "unix:${socket}" = { }; };
    };
  vhostFromEnv = name: env:
    let
      primary = env.hostname or name;
      baseVH = env.virtualHosts or { };
      extras = env.extras.nginx or { };
      defaultLoc = {
        "/" = {
          recommendedProxySettings = true;
          proxyPass = "http://${name}_upstream";
        };
      };
      addPrimary = baseVH // {
        ${primary} = (baseVH.${primary} or { }) // {
          locations = (baseVH.${primary}.locations or { }) // defaultLoc;
          forceSSL = lib.mkDefault true;
          enableACME = lib.mkDefault true;
          extraConfig = extras.extraConfig or "";
          http2 = true;
          hsts = extras.hsts or true;
          serverAliases = extras.aliases or [ ];
          # Simple header set (extendable via extras)
          extraHeaders = extras.headers or {
            "Strict-Transport-Security" = "max-age=63072000; includeSubDomains; preload";
            "X-Frame-Options" = "SAMEORIGIN";
            "X-Content-Type-Options" = "nosniff";
          };
          securityHeaders = lib.mkIf (extras ? csp) {
            "Content-Security-Policy" = extras.csp;
          };
        };
      };
    in addPrimary;
in
{
  config = lib.mkIf (envs != { }) {
    assertions = lib.flatten (lib.mapAttrsToList (n: env: [
      {
        assertion = (env.upstreamRuntimeDir or "") != "";
        message = "hostenv nginx: upstreamRuntimeDir missing for env ${n}";
      }
    ]) envs);

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = lib.foldl' lib.recursiveUpdate { }
        (lib.mapAttrsToList (n: env: vhostFromEnv (env.user or n) env) envs);
      upstreams = lib.listToAttrs (lib.mapAttrsToList
        (n: env:
          let user = env.user or n;
          in {
            name = "${user}_upstream";
            value = mkUpstream n env;
          })
        envs);
    };
  };
}
