{ pkgs }:

let
  lib = pkgs.lib;

  sliceEval =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          ({ lib, ... }: {
            options.hostenv = lib.mkOption {
              type = lib.types.submodule {
                freeformType = lib.types.attrs;
                options.defaultEnvironment = lib.mkOption { type = lib.types.str; default = "main"; };
              };
              default = { };
            };
            options.systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.users = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.groups = lib.mkOption { type = lib.types.attrs; default = { }; };
          })
          ../modules/core/environments.nix
          ../modules/nixos/users-slices.nix
          ({ config, ... }: {
            _module.check = false;
            environments = {
              alpha = {
                _module.check = false;
                enable = true;
                user = "alpha";
                extras.publicKeys = [ ];
                extras.uid = 123;
              };
            };
            hostenv.environments = config.environments;
          })
        ];
      };
      slice = eval.config.systemd.slices.alpha;
      sliceJson = builtins.toFile "slice.json" (builtins.toJSON slice);
    in pkgs.runCommand "users-slices-configured" { } ''
      cp ${sliceJson} $out
      grep -q '"CPUAccounting":"yes"' $out
      grep -q '"MemoryMax":"12G"' $out
    '';

  disabledEval =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          ({ lib, ... }: {
            options.hostenv = lib.mkOption {
              type = lib.types.submodule { freeformType = lib.types.attrs; };
              default = { };
            };
            options.services.nginx = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };
          })
          ../modules/core/environments.nix
          ../modules/nixos/nginx-hostenv.nix
          ({ config, ... }: {
            _module.check = false;
            environments = {
              on = {
                _module.check = false;
                enable = true;
                user = "onuser";
                hostname = "on.example";
                upstreamRuntimeDir = "/run/hostenv/nginx/onuser";
                virtualHosts = { };
              };
              off = {
                _module.check = false;
                enable = false;
                user = "offuser";
                hostname = "off.example";
                upstreamRuntimeDir = "/run/hostenv/nginx/offuser";
                virtualHosts = { };
              };
            };
            hostenv.environments = config.environments;
          })
        ];
      };
      upstreams = eval.config.services.nginx.upstreams;
      _mustHave = if upstreams ? onuser_upstream then null else builtins.throw "onuser_upstream missing";
      _mustNotHave = if builtins.any (n: lib.hasInfix "offuser" n) (builtins.attrNames upstreams)
        then builtins.throw "disabled env present in upstreams"
        else null;
    in pkgs.runCommand "disabled-envs-filtered" { } ''
      echo ok > $out
    '';
in {
  slice_defaults_applied = sliceEval;
  disabled_envs_filtered = disabledEval;
}
