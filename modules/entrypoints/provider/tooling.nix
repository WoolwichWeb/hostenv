{ inputs, lib, config, ... }:
let
  flakeParts = inputs.flake-parts.lib;
  cfgTop = config.provider;
  hasProviderConfig = config.provider.enable;
  providerRoot = ../../../provider;
  cliPath = providerRoot + /cli.hs;

  hostenvInput =
    if inputs ? hostenv then inputs.hostenv
    else if inputs ? self then inputs.self
    else throw "provider tooling requires a hostenv input";

in
{
  options.perSystem = flakeParts.mkPerSystemOption ({ lib, ... }: {
    options.provider.haskellDevPackages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra Haskell package names to include in the dev shell for provider tooling.";
    };
  });

  config.perSystem = { system, pkgs, ... }:
    lib.mkIf cfgTop.enable
      (let
        providerHsPackageNames = [
          "aeson"
          "aeson-pretty"
          "text"
          "text-conversions"
          "bytestring"
          "optparse-applicative"
          "turtle"
        ];
        ghcPackageNames = lib.unique config.hostenv.haskell.devPackages;
        devGhc = pkgs.haskellPackages.ghcWithPackages (p: map (name: p.${name}) ghcPackageNames);
        providerHsDeps = p: map (name: p.${name}) providerHsPackageNames;
        providerGhc = pkgs.haskellPackages.ghcWithPackages providerHsDeps;
        cliSrc = builtins.path { path = cliPath; name = "hostenv-provider-cli"; };
        hostenvProviderCLI = pkgs.writeShellScriptBin "hostenv-provider" ''
          set -euo pipefail
          exec ${providerGhc}/bin/runghc ${cliSrc} "$@"
        '';

        providerPlan = config.flake.lib.provider.plan;
        providerGenerator =
          if !hasProviderConfig then null else
          providerPlan {
            inputs = inputs // { hostenv = hostenvInput; };
            inherit system;
            lib = pkgs.lib;
            pkgs = pkgs;
            letsEncrypt = cfgTop.letsEncrypt;
            deployPublicKeys = cfgTop.deployPublicKeys;
            hostenvHostname = cfgTop.hostenvHostname;
            nodeFor = cfgTop.nodeFor;
            nodeSystems = cfgTop.nodeSystems;
            nodeModules = cfgTop.nodeModules;
            statePath = cfgTop.statePath;
            planPath = cfgTop.planPath;
            cloudflare = cfgTop.cloudflare;
            planSource = cfgTop.planSource;
            generatedFlake = cfgTop.generatedFlake;
          };

        hostenvProviderPlan =
          if hasProviderConfig then
            pkgs.writeShellScriptBin "hostenv-provider-plan" ''
              set -euo pipefail
              dest=generated
              mkdir -p "$dest"

              # The following are:
              # - a sub-Flake with per-environment inputs (inputs in
              #   provider Flakes are per-project);
              cp ${providerGenerator.flake} "$dest/flake.nix"

              # - state retained between runs, like unique UNIX user uids.
              cat ${providerGenerator.state} > "$dest/state.json"

              # - a plan for the next deployment, containing Nix config and
              #   and environment metadata. This file is more ephemeral than
              #   the other two, and does not need to be committed to git (but
              #   still needs to be git add-ed for Nix flakes to see it).)
              cat ${providerGenerator.plan} > "$dest/plan.json"

              chmod ug+w "$dest"/{state.json,plan.json,flake.nix}
              (cd "$dest" && nix --extra-experimental-features "nix-command flakes" flake update)
              git add "$dest" || true

              echo "✅ Infrastructure configuration written to $dest"
            ''
          else
            pkgs.writeShellScriptBin "hostenv-provider-plan" ''
              echo "hostenv-provider-plan: provider configuration is missing." >&2
              echo "Import hostenv modules via hostenv.flakeModules.provider, then set provider.* options." >&2
              exit 1
            '';
      in
      {
        packages = {
          hostenv-provider = hostenvProviderCLI;
          hostenv-provider-plan = hostenvProviderPlan;
        };

        apps.hostenv-provider-plan = {
          type = "app";
          program = "${hostenvProviderPlan}/bin/hostenv-provider-plan";
          meta.description = "Generate provider plan/state/flake";
        };

        provider.haskellDevPackages = providerHsPackageNames;
        hostenv.haskell.devPackages = lib.mkDefault (
          [ "haskell-language-server" ] ++ config.provider.haskellDevPackages
        );
        devShells.default = lib.mkDefault (pkgs.mkShell {
          inputsFrom = config.hostenv.devShell.inputsFrom;

          buildInputs =
            (lib.optional cfgTop.enable config.packages.hostenv-provider)
            ++ (lib.optional cfgTop.enable config.packages.hostenv-provider-plan)
            ++ [
              pkgs.sops
              pkgs.age
              pkgs.jq
              pkgs.bind
              pkgs.deploy-rs
              pkgs.git
              pkgs.postgresql
              pkgs.socat
              pkgs.haskellPackages.cabal-install
              devGhc
            ]
            ++ config.hostenv.devShell.packages;

          shellHook = ''
            if command -v ghc >/dev/null; then
              libdir="$(ghc --print-libdir)"
              pkgdb="''${libdir}/package.conf.d"
              cat > "$PWD/cabal.project.local" <<EOF
-- Generated by nix develop (provider tooling)
package-dbs: clear, ''${pkgdb}
with-compiler: ghc
EOF
            fi
          '' + "\n" + config.hostenv.devShell.shellHook;
        });
      });
}
