{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };
  envFixtures = import ../environments.nix { inherit pkgs makeHostenv; };
  exportedEnvs = envFixtures.drupalProduction.config.exportedEnvironments;
  mainEnv = exportedEnvs.main or { };
  hostenvMeta = mainEnv.hostenv or { };
  jsonEval = builtins.tryEval (builtins.toJSON exportedEnvs);

  noPriority = !(mainEnv ? priority);
  noAssertions = !(hostenvMeta ? assertions);
  usersPresent = (mainEnv.users or { }) != { };
in
asserts.assertTrue "public-env-json-sanitized"
  (noPriority && noAssertions && usersPresent && jsonEval.success)
  "exported environment JSON should exclude internal fields and remain JSON-serializable"
