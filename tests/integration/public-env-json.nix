{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };
  envFixtures = import ../environments.nix { inherit pkgs makeHostenv; };
  publicEnvs = envFixtures.drupalProduction.config.hostenv.publicEnvironments;
  mainEnv = publicEnvs.main or { };
  hostenvMeta = mainEnv.hostenv or { };
  jsonEval = builtins.tryEval (builtins.toJSON publicEnvs);

  noPriority = !(mainEnv ? priority);
  noAssertions = !(hostenvMeta ? assertions);
  usersPresent = (mainEnv.users or { }) != { };
  jsonOk = jsonEval.success;
in
asserts.assertTrue "public-env-json-sanitized"
  (noPriority && noAssertions && usersPresent && jsonOk)
  "public environment JSON should exclude internal fields and remain JSON-serializable"
