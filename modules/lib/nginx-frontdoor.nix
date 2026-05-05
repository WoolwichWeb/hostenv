{ ... }:
let
  mkNodeNginxConfig =
    { lib
    , envs
    , runtimeRoot
    , defaultEnableLetsEncrypt
    , mkDefault ? (x: x)
    }:
    let
      enabledEnvs = lib.filterAttrs (_: env: env.enable != false) envs;

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
          socket = (env.hostenv.upstreamRuntimeDir or "${runtimeRoot}/nginx/${user}") + "/in.sock";
        in
        {
          servers = { "unix:${socket}" = { }; };
        };

      vhostFromEnv = envName: env:
        let
          user = env.hostenv.userName or envName;
          primary = env.hostenv.hostname or user;
          normalizeVhost = vhostName: vhost:
            let
              enableLetsEncrypt =
                vhost.enableLetsEncrypt
                  or (if vhostName == primary then defaultEnableLetsEncrypt else false);
              forceSSL =
                vhost.forceSSL
                  or enableLetsEncrypt;
              tlsEnabled = enableLetsEncrypt || forceSSL;
              headerConfig = mkSecurityHeaders { vhost = vhost; envType = env.type; tlsEnabled = tlsEnabled; };
            in
            (builtins.removeAttrs vhost [
              "enableLetsEncrypt"
              "enableACME"
              "allowIndexing"
              "security"
              "hsts"
            ]) // {
              enableACME = mkDefault enableLetsEncrypt;
              forceSSL = mkDefault forceSSL;
              extraConfig = lib.concatStringsSep "\n" (lib.filter (line: line != "") [
                (vhost.extraConfig or "")
                headerConfig
              ]);
              locations =
                (vhost.locations or { }) // (
                  if (vhost.globalRedirect or null) == null then {
                    "/" = {
                      recommendedProxySettings = true;
                      proxyPass = "http://${user}_upstream";
                    };
                  } else { }
                );
            } // lib.optionalAttrs (vhostName == primary) {
              http2 = true;
            };

          addPrimary = (env.virtualHosts or { }) // {
            ${primary} = (env.virtualHosts or { }).${primary} or { };
          };
        in
        lib.mapAttrs normalizeVhost addPrimary;
    in
    {
      networking.firewall.allowedTCPPorts = [ 80 443 22 ];

      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        virtualHosts =
          (lib.foldl' lib.recursiveUpdate { }
            (
              lib.mapAttrsToList
                (n: env: vhostFromEnv (env.hostenv.userName or n) env)
                enabledEnvs
            ))
          // {
            default = {
              serverName = "_";
              default = true;
              rejectSSL = true;
              locations."/".return = "444";
            };
          };
        upstreams = lib.listToAttrs (lib.mapAttrsToList
          (n: env:
            let user = env.hostenv.userName or n;
            in {
              name = "${user}_upstream";
              value = mkUpstream n env;
            })
          enabledEnvs);
      };
    };
in
{
  flake.lib.hostenv.nginxFrontdoor = {
    inherit mkNodeNginxConfig;
  };
}
