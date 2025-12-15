{ pkgs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  samples = support.samples;
  asserts = support.asserts;

  slice_eval = support.evalWithBase {
    specialArgs = { inherit pkgs; };
    modules = [
      ../../modules/core/environments.nix
      ../../modules/nixos/plan-bridge.nix
      ../../modules/nixos/users-slices.nix
      ({ ... }: {
        _module.check = false;
        environments = samples.mkSingle {
          name = "alpha";
          userName = "alpha";
          publicKeys = [ ];
        };
        defaultEnvironment = "alpha";
      })
    ];
  };

  slice_defaults_applied =
    let
      slice = lib.head (lib.attrValues slice_eval.config.systemd.slices);
      cfg = slice.sliceConfig or { };
      ok = (cfg.CPUAccounting or "") == "yes"
        && (builtins.match "^[0-9]+G$" (cfg.MemoryMax or "")) != null;
    in asserts.assertTrue "users-slices-configured" ok "slice defaults not applied";

  slice_respects_custom_user =
    let
      eval = support.evalWithBase {
        specialArgs = { inherit pkgs; };
        modules = [
          ../../modules/core/environments.nix
          ../../modules/nixos/plan-bridge.nix
          ../../modules/nixos/users-slices.nix
          ({ ... }: {
            _module.check = false;
            environments = samples.mkSingle {
              name = "envWithCustomUser";
              userName = "customuser";
              publicKeys = [ ];
            };
            defaultEnvironment = "envWithCustomUser";
          })
        ];
      };
      slices = eval.config.systemd.slices;
      users = eval.config.users.users;
      sliceName = lib.head (lib.attrNames slices);
      userName = lib.head (lib.attrNames users);
      ok = (lib.removeSuffix ".slice" sliceName) == userName;
    in asserts.assertTrue "users-slices-custom-user" ok "custom user slice/user mismatch";

in {
  inherit slice_defaults_applied slice_respects_custom_user;
}
