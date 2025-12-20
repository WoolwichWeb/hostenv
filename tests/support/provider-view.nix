{ lib }:
let
  # List-of-projects view for provider/plan.nix inputs.
  toProjects =
    { envs
    , baseRepo
    , node ? "node1"
    , hostenvHostname ? "hosting.test"
    , rootBase ? "/src"
    }:
    lib.attrValues (lib.mapAttrs
      (_: envCfg: {
        hostenv =
          envCfg.hostenv // {
            gitRef = envCfg.hostenv.gitRef or baseRepo.ref;
            hostenvHostname = hostenvHostname;
            root = "${rootBase}/${envCfg.hostenv.project or "proj"}";
          };
        inherit node;
        authorizedKeys =
          let allUsers = builtins.attrValues (envCfg.users or { }); in
          builtins.concatLists (map (u: u.publicKeys or [ ]) allUsers);
        type = envCfg.type;
        users = envCfg.users or { };
        virtualHosts = envCfg.virtualHosts or { };
        repo = baseRepo // { ref = envCfg.hostenv.gitRef or baseRepo.ref; };
      })
      envs);
in
{
  inherit toProjects;
}
