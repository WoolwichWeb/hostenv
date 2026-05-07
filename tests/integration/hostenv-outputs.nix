{ pkgs, makeHostenv, inputs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  system = pkgs.stdenv.hostPlatform.system;
  hostenvModules = inputs.import-tree ../../modules;
  moduleList = if builtins.isList hostenvModules then hostenvModules else [ hostenvModules ];

  mkOutputs = { modules, environmentName ? null }:
    let
      flake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ system ];
        imports = [ inputs.devshell.flakeModule ] ++ moduleList;

        project.enable = true;

        perSystem = { ... }: {
          hostenvProject = {
            makeHostenv = lib.mkForce makeHostenv;
            modules = lib.mkForce modules;
            environmentName = lib.mkForce environmentName;
          };
        };
      };
    in
    flake.lib.hostenv.${system};

  outputs = mkOutputs {
    modules = [
      ({ ... }: {
        hostenv = {
          organisation = "acme";
          project = "demo";
          hostenvHostname = "hosting.test";
          root = ./.;
        };
        environments.main = {
          enable = true;
          type = "production";
        };
      })
    ];
    environmentName = "main";
  };

  productionFallbackOutputs = mkOutputs {
    modules = [
      ({ ... }: {
        hostenv = {
          organisation = "acme";
          project = "fallback";
          hostenvHostname = "hosting.test";
          root = ./.;
        };
        environments.production = {
          enable = true;
          type = "production";
        };
      })
    ];
  };
  productionFallbackEval = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "fallback";
        hostenvHostname = "hosting.test";
        root = ./.;
      };
      environments.production = {
        enable = true;
        type = "production";
      };
    })
  ] null;

  ok =
    outputs ? environments
    && outputs ? defaultEnvironment
    && (productionFallbackOutputs.defaultEnvironment or null) == "production"
    && (productionFallbackOutputs.environments ? production)
    && productionFallbackEval.config.defaultEnvironment == "production"
    && productionFallbackEval.config.hostenv.environmentName == "production";
in
asserts.assertTrue "hostenv-outputs-eval"
  ok
  "lib.hostenv outputs and makeHostenv null evals should preserve the single-production default fallback"
