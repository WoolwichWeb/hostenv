{ ... }:
{
  flake.modules.nixos.nginx-frontdoor =
    { lib, pkgs, config, ... }:
    let
      allEnvs = config.hostenv.environments or { };
      # Treat enable = false as disabled; default to enabled when unset.
      envs = lib.filterAttrs (_: env: env.enable != false) allEnvs;
      sanitizeHeaderValue = label: value:
        if value == null then null else
        if lib.strings.hasInfix "\"" value then
          builtins.throw "hostenv nginx: ${label} may not contain double quotes"
        else
          value;
      mkHeaderLine = name: value:
        if value == null then ""
        else
          let
            hasDouble = lib.strings.hasInfix "\"" value;
            hasSingle = lib.strings.hasInfix "'" value;
            quote =
              if hasDouble && !hasSingle then "'"
              else if hasDouble && hasSingle then
                builtins.throw "hostenv nginx: ${name} header value may not contain both single and double quotes"
              else
                "\"";
          in
          ''add_header ${name} ${quote}${value}${quote} always;'';
      mkSecurityHeaders = { vhost, envType, tlsEnabled }:
        let
          security = vhost.security or { };
          referrerPolicy = security.referrerPolicy or "strict-origin-when-cross-origin";
          xFrameOptions = security.xFrameOptions or "SAMEORIGIN";
          xContentTypeOptions = security.xContentTypeOptions or true;
          reportTo = security.reportTo or null;
          hstsEnabled = (security.hsts or (vhost.hsts or true)) && tlsEnabled;
          cspBase = sanitizeHeaderValue "security.csp" (security.csp or null);
          cspReportTo = security.cspReportTo or null;
          cspValue =
            if cspBase == null then null else
            let trimmed = lib.strings.removeSuffix ";" cspBase;
            in
            if cspReportTo == null then
              trimmed
            else
              "${trimmed}; report-to ${cspReportTo}";
          cspHeaderName =
            if (security.cspMode or "enforce") == "report-only" then
              "Content-Security-Policy-Report-Only"
            else
              "Content-Security-Policy";
          allowIndexing = vhost.allowIndexing or (envType == "production");
        in
        lib.concatStringsSep "\n" (lib.filter (line: line != "") [
          (mkHeaderLine "Report-To" reportTo)
          (mkHeaderLine "Referrer-Policy" referrerPolicy)
          (if xFrameOptions == null then "" else mkHeaderLine "X-Frame-Options" xFrameOptions)
          (if xContentTypeOptions then mkHeaderLine "X-Content-Type-Options" "nosniff" else "")
          (if hstsEnabled then mkHeaderLine "Strict-Transport-Security" "max-age=63072000; includeSubDomains; preload" else "")
          (if cspValue == null then "" else mkHeaderLine cspHeaderName cspValue)
          (if allowIndexing then "" else mkHeaderLine "X-Robots-Tag" "noindex")
        ]);
      mkUpstream = envName: env:
        let
          user = env.hostenv.userName or envName;
          socket = (env.hostenv.upstreamRuntimeDir or "${config.hostenv.runtimeRoot}/nginx/${user}") + "/in.sock";
        in
        {
          servers = { "unix:${socket}" = { }; };
        };
      vhostFromEnv = name: env:
        let
          primary = env.hostenv.hostname or name;
          baseVH = env.virtualHosts or { };
          enableACMEDefault = config.hostenv.nginx.enableACME;
          defaultLoc = {
            "/" = {
              recommendedProxySettings = true;
              proxyPass = "http://${name}_upstream";
            };
          };
          normalizeVhost = vhostName: vhost:
            let
              enableACME =
                vhost.enableACME
                  or vhost.enableLetsEncrypt
                  or (if vhostName == primary then enableACMEDefault else false);
              forceSSL =
                vhost.forceSSL
                  or vhost.enableACME
                  or vhost.enableLetsEncrypt
                  or (if vhostName == primary then enableACMEDefault else false);
              tlsEnabled = enableACME || forceSSL;
              headerConfig = mkSecurityHeaders { vhost = vhost; envType = env.type; tlsEnabled = tlsEnabled; };
              baseConfig =
                (builtins.removeAttrs vhost [ "enableLetsEncrypt" "allowIndexing" "security" "hsts" ])
                // {
                  enableACME = lib.mkDefault enableACME;
                  forceSSL = lib.mkDefault forceSSL;
                  extraConfig = lib.concatStringsSep "\n" (lib.filter (line: line != "") [
                    (vhost.extraConfig or "")
                    headerConfig
                  ]);
                };
            in
            if vhostName == primary then
              baseConfig // {
                locations = (vhost.locations or { }) // defaultLoc;
                http2 = true;
              }
            else
              baseConfig;

          addPrimary = baseVH // {
            ${primary} = baseVH.${primary} or { };
          };
        in
        lib.mapAttrs normalizeVhost addPrimary;
    in
    {
      options.hostenv.nginx.enableACME = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether host-level nginx should request ACME certs by default.";
      };

      config = lib.mkIf (envs != { }) {
        assertions = lib.flatten (lib.mapAttrsToList
          (n: env: [
            {
              assertion = (env.hostenv.upstreamRuntimeDir or "") != "";
              message = "hostenv nginx: upstreamRuntimeDir missing for env ${n}";
            }
          ])
          envs);

        # Keep these ports in the merged result even when other modules
        # (for example sshd openFirewall) contribute non-default values.
        networking.firewall.allowedTCPPorts = [ 80 443 22 ];

        services.nginx = {
          enable = true;
          recommendedProxySettings = true;
          virtualHosts = lib.foldl' lib.recursiveUpdate { }
            (lib.mapAttrsToList (n: env: vhostFromEnv (env.hostenv.userName or n) env) envs);
          upstreams = lib.listToAttrs (lib.mapAttrsToList
            (n: env:
              let user = env.hostenv.userName or n;
              in {
                name = "${user}_upstream";
                value = mkUpstream n env;
              })
            envs);
        };
      };
    }
  ;
}
