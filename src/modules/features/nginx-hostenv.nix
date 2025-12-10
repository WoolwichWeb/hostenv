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
          extraConfig = env.extras.nginx.extraConfig or "";
          http2 = true;
          hsts = env.extras.nginx.hsts or true;
          serverAliases = env.extras.nginx.aliases or [ ];
          # Simple header set (extendable via extras)
          extraHeaders = env.extras.nginx.headers or {
            "Strict-Transport-Security" = "max-age=63072000; includeSubDomains; preload";
            "X-Frame-Options" = "SAMEORIGIN";
            "X-Content-Type-Options" = "nosniff";
          };
          securityHeaders = lib.mkIf (env.extras.nginx ? csp) {
            "Content-Security-Policy" = env.extras.nginx.csp;
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
      upstreams = lib.mapAttrs (n: env: mkUpstream n env) envs;
    };
  };
}
