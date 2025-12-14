{ lib }:
# Adapt environment fixtures into the provider-style shape expected by plan-bridge
# and host-level modules.
envs:
  lib.mapAttrs (_: env: {
    inherit (env) enable type virtualHosts users;
    hostenv = env.hostenv // { extras = env.hostenv.extras or { }; };
  }) envs
