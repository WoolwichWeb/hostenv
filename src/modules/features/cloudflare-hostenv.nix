{ lib, config, pkgs, ... }:
let
  cfg = config.hostenv.cloudflare;
  envs = config.hostenv.environments or {};
in
{
  options.hostenv.cloudflare = {
    enable = lib.mkEnableOption "Cloudflare DNS upsert for hostenv vhosts" // { default = false; };
    zoneId = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Cloudflare zone ID";
    };
    apiTokenFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to file containing CF_API_TOKEN";
    };
  };

  config = lib.mkIf (cfg.enable && cfg.zoneId != null && cfg.apiTokenFile != null && envs != {}) {
    assertions = [
      { assertion = cfg.zoneId != null; message = "hostenv.cloudflare.zoneId is required"; }
      { assertion = cfg.apiTokenFile != null; message = "hostenv.cloudflare.apiTokenFile is required"; }
    ];
  };
}
