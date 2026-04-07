{ pkgs, envs, makeHostenv, inputs }:

(import ./integration { inherit pkgs envs makeHostenv inputs; })
  // (import ./unit { inherit pkgs makeHostenv inputs; })
