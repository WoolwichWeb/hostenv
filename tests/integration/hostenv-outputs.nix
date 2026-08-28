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
  ] "production";

  nullMakeHostenvFails = !(builtins.tryEval (makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "explicit-only";
        hostenvHostname = "hosting.test";
        root = ./.;
      };
      environments.bonza.enable = true;
    })
  ] null)).success;

  unknownMakeHostenvFails = !(builtins.tryEval (makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "explicit-only";
        hostenvHostname = "hosting.test";
        root = ./.;
      };
      environments.bonza.enable = true;
    })
  ] "main")).success;

  singleTestingOutputs = mkOutputs {
    modules = [
      ({ ... }: {
        hostenv = {
          organisation = "acme";
          project = "bonza-project";
          hostenvHostname = "hosting.test";
          root = ./.;
        };
        environments.bonza = {
          enable = true;
          type = "testing";
        };
      })
    ];
  };

  emptyOutputs = mkOutputs {
    modules = [
      ({ ... }: {
        hostenv = {
          organisation = "acme";
          project = "empty-project";
          hostenvHostname = "hosting.test";
          root = ./.;
        };
        # Removing every environment should remain valid even if an old explicit
        # default is left behind in the project configuration.
        defaultEnvironment = "main";
      })
    ];
  };

  multipleTestingOutputs = mkOutputs {
    modules = [
      ({ ... }: {
        hostenv = {
          organisation = "acme";
          project = "ambiguous-project";
          hostenvHostname = "hosting.test";
          root = ./.;
        };
        environments = {
          bonza.enable = true;
          cobber.enable = true;
        };
      })
    ];
  };

  ok =
    outputs ? environments
    && outputs ? defaultEnvironment
    && (productionFallbackOutputs.defaultEnvironment or null) == "production"
    && (productionFallbackOutputs.environments ? production)
    && productionFallbackEval.config.defaultEnvironment == "production"
    && productionFallbackEval.config.hostenv.environmentName == "production"
    && nullMakeHostenvFails
    && unknownMakeHostenvFails
    && singleTestingOutputs.defaultEnvironment == "bonza"
    && (singleTestingOutputs.environments ? bonza)
    && !(singleTestingOutputs.environments ? main)
    && emptyOutputs.defaultEnvironment == null
    && emptyOutputs.environments == { }
    && multipleTestingOutputs.defaultEnvironment == "bonza"
    && (builtins.attrNames multipleTestingOutputs.environments) == [ "bonza" "cobber" ];
in
asserts.assertTrue "hostenv-outputs-eval"
  ok
  "project discovery should select real defaults without synthesizing main, and allow projects with no default"
