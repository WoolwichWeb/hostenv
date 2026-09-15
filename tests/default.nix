{ pkgs, envs, makeHostenv, inputs, documentation }:

(import ./integration { inherit pkgs envs makeHostenv inputs; })
  // (import ./unit { inherit pkgs makeHostenv documentation; })
