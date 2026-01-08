{ ... }:
{
  flake.modules.nixos.nginx-tuning =
    # Host-level nginx tuning defaults, matching previous provider overrides,
    # applied when hostenv environments are present.
    { lib, config, ... }:
    let
      envs = config.hostenv.environments or { };
      cfg = config.hostenv.nginxTuning;
    in
    {
      options.hostenv.nginxTuning = {
        enable = lib.mkEnableOption "apply hostenv nginx tuning defaults" // { default = true; };

        clientMaxBodySize = lib.mkOption {
          type = lib.types.str;
          default = "2G";
          description = "Default client_max_body_size for hostenv front-door nginx.";
        };

        proxyTimeout = lib.mkOption {
          type = lib.types.str;
          default = "600s";
          description = "Default proxy_read_timeout / proxy_connect_timeout.";
        };

        appendHttpConfig = lib.mkOption {
          type = lib.types.lines;
          default = ''
            proxy_buffer_size   128k;
            proxy_buffers   4 256k;
            proxy_busy_buffers_size   256k;
          '';
          description = "Extra HTTP config appended to nginx http {} block.";
        };

        commonHttpConfig = lib.mkOption {
          type = lib.types.lines;
          default = ''
            log_format upstream_params '[$time_local] $remote_addr - $server_port - $remote_user - $host - $proxy_add_x_forwarded_for - $scheme to: $upstream_addr ($request $status $upstream_response_time $upstream_response_time msec $msec request_time $request_time)';
            access_log syslog:server=unix:/dev/log upstream_params;
          '';
          description = "Common HTTP config prepended inside http {}.";
        };
      };

      config = lib.mkIf (cfg.enable && envs != { }) {
        services.nginx = {
          clientMaxBodySize = lib.mkDefault cfg.clientMaxBodySize;
          proxyTimeout = lib.mkDefault cfg.proxyTimeout;
          appendHttpConfig = lib.mkDefault cfg.appendHttpConfig;
          commonHttpConfig = lib.mkDefault cfg.commonHttpConfig;
        };
      };
    }

  ;
}
