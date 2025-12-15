{ pkgs, envs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  checkUpstream = env:
    let
      user = env.config.hostenv.userName;
      runtimeDir = env.config.hostenv.upstreamRuntimeDir;
    in asserts.assertTrue "${user}-nginx-upstream-socket"
      (runtimeDir != null && runtimeDir != "")
      "upstreamRuntimeDir should be set for ${user}";

  checkPhpSockets = env:
    let
      user = env.config.hostenv.userName;
      runtimeDir = env.config.hostenv.runtimeDir;
    in asserts.assertTrue "${user}-phpfpm-socket-paths"
      (runtimeDir != null && runtimeDir != "")
      "runtimeDir should be set for ${user}";

in {
  drupal_dev_nginx_upstream_socket = checkUpstream envs.drupalDev;
  drupal_prod_nginx_upstream_socket = checkUpstream envs.drupalProduction;
  drupal_dev_phpfpm_socket_paths = checkPhpSockets envs.drupalDev;
  drupal_prod_phpfpm_socket_paths = checkPhpSockets envs.drupalProduction;
}
