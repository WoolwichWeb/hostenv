{ pkgs, ... }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  eval = support.evalWithBase {
    modules = [
      ({ lib, ... }: {
        options.flake = {
          modules = lib.mkOption {
            type = lib.types.attrsOf lib.types.unspecified;
            default = { };
          };
          lib = lib.mkOption {
            type = lib.types.attrsOf lib.types.unspecified;
            default = { };
          };
        };

        config = {
          _module.args.inputs = { self = ./.; };
          flake = {
            modules.nixos = {
              provider-common = { };
              hostenv-top-level = { };
              nginx-tuning = { };
              monitoring = { };
            };
            lib.hostenv.mkServiceResolutionOption = { lib }: lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
        };
      })
      ../../modules/entrypoints/provider/options.nix
      {
        provider.enable = true;
        provider.hostenvHostname = "hosting.test";
      }
    ];
  };

  cfg = eval.config.provider;
  ok =
    cfg.nodeAddresses == { }
    && cfg.nodeSshPorts == { }
    && cfg.nodeSshOpts == { }
    && cfg.nodeRemoteBuild == { }
    && cfg.nodeMagicRollback == { }
    && cfg.nodeAutoRollback == { };
in
asserts.assertTrue "provider-plan-paths-eval"
  ok
  "provider options should keep legacy node SSH override attrs with empty defaults while provider-service rollout is incomplete"
