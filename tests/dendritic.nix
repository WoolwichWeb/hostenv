{ pkgs }:

let
  lib = pkgs.lib;

  sliceEval =
    let
      eval = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          ({ lib, ... }: {
            options.hostenv = lib.mkOption { type = lib.types.submodule { }; default = { }; };
            options.systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.users = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.users.groups = lib.mkOption { type = lib.types.attrs; default = { }; };
          })
          ../modules/core/hostenv.nix
          ../modules/core/environments.nix
          ../modules/nixos/users-slices.nix
          ({ ... }: {
            environments = {
              alpha = {
                enable = true;
                user = "alpha";
                extras.publicKeys = [ ];
                extras.uid = 123;
              };
            };
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
            options.hostenv = lib.mkOption { type = lib.types.submodule { }; default = { }; };
            options.systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
            options.systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };
          })
          ../modules/core/hostenv.nix
          ../modules/core/environments.nix
          ../modules/nixos/nginx-hostenv.nix
          ({ ... }: {
            environments = {
              on = {
                enable = true;
                user = "onuser";
                hostname = "on.example";
                upstreamRuntimeDir = "/run/hostenv/nginx/onuser";
                virtualHosts = { };
              };
              off = {
                enable = false;
                user = "offuser";
                hostname = "off.example";
                upstreamRuntimeDir = "/run/hostenv/nginx/offuser";
                virtualHosts = { };
              };
            };
          })
        ];
      };
      upstreams = eval.config.services.nginx.upstreams;
      upstreamsJson = builtins.toFile "upstreams.json" (builtins.toJSON upstreams);
    in pkgs.runCommand "disabled-envs-filtered" { } ''
      cp ${upstreamsJson} $out
      grep -q '"onuser_upstream"' $out
      if grep -q 'offuser' $out; then
        echo "FAIL: disabled env 'off' still present in upstreams" >&2
        exit 1
      fi
    '';
in {
  slice_defaults_applied = sliceEval;
  disabled_envs_filtered = disabledEval;
}
