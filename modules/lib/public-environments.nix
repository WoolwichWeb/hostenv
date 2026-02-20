{ lib, ... }:
let
  publicEnvironments = envs:
    let
      hostenvPublic = env:
        let h = env.hostenv or { };
        in {
          organisation = h.organisation or null;
          project = h.project or null;
          hostenvHostname = h.hostenvHostname or null;
          environmentName = h.environmentName or null;
          safeEnvironmentName = h.safeEnvironmentName or null;
          gitRef = h.gitRef or null;
          userName = h.userName or null;
          hostname = h.hostname or null;
        };

      envPublic = env: {
        enable = env.enable or true;
        type = env.type or "development";
        users = env.users or { };
        virtualHosts = env.virtualHosts or { };
        secrets = env.secrets or { };
        hostenv = hostenvPublic env;
      };
    in
    lib.mapAttrs (_: env: envPublic env) envs;
in
{
  config.flake.lib.hostenv.publicEnvironments = publicEnvironments;
}
