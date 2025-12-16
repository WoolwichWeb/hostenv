# Also see:
# https://github.com/NixOS/nixpkgs/blob/1b42cb3018b81be411825ec5beadac8ce89c0633/nixos/modules/services/web-servers/nginx/default.nix#L323
# This module fills the same Nix options, but has been updated.
# @todo: refactor this and the use of upstream module from nixpkgs
# (in nginx.nix).
{ lib, config, virtualHosts, enableRouteDebugging, pkgs }:

let
  inherit (lib)
    optionalString concatStringsSep mapAttrsToList optionalAttrs sortProperties
    concatMapStringsSep;

  cfg = config.services.nginx;

  defaultFastcgiParams = {
    SCRIPT_FILENAME = "$document_root$fastcgi_script_name";
    QUERY_STRING = "$query_string";
    REQUEST_METHOD = "$request_method";
    CONTENT_TYPE = "$content_type";
    CONTENT_LENGTH = "$content_length";

    SCRIPT_NAME = "$fastcgi_script_name";
    REQUEST_URI = "$request_uri";
    DOCUMENT_URI = "$document_uri";
    DOCUMENT_ROOT = "$document_root";
    SERVER_PROTOCOL = "$server_protocol";
    REQUEST_SCHEME = "$scheme";
    HTTPS = "$https if_not_empty";

    GATEWAY_INTERFACE = "CGI/1.1";
    SERVER_SOFTWARE = "nginx/$nginx_version";

    REMOTE_ADDR = "$remote_addr";
    REMOTE_PORT = "$remote_port";
    SERVER_ADDR = "$server_addr";
    SERVER_PORT = "$server_port";
    SERVER_NAME = "$server_name";

    REDIRECT_STATUS = "200";
  };

  recommendedProxyConfig = pkgs.writeText "nginx-recommended-proxy_set_header-headers.conf" ''
    proxy_set_header        Host $host;
    proxy_set_header        X-Real-IP $remote_addr;
    proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header        X-Forwarded-Proto $scheme;
    proxy_set_header        X-Forwarded-Host $host;
    proxy_set_header        X-Forwarded-Server $hostname;
  '';

  hostenvExtraForLocation = config: optionalString enableRouteDebugging
    ''set $hostenv_handled "${sanitizeForHeader config.location}";'';

  mkLocations = locations: concatStringsSep "\n" (map
    (config: ''
      location ${config.location} {
        ${hostenvExtraForLocation config}
        ${optionalString (
          config.proxyPass != null && !cfg.proxyResolveWhileRunning
        ) "proxy_pass ${config.proxyPass};"}
        ${optionalString (config.proxyPass != null && cfg.proxyResolveWhileRunning) ''
          set $nix_proxy_target "${config.proxyPass}";
          proxy_pass $nix_proxy_target;
        ''}
        ${optionalString config.proxyWebsockets ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection $connection_upgrade;
        ''}
        ${optionalString (
          config.uwsgiPass != null && !cfg.uwsgiResolveWhileRunning
        ) "uwsgi_pass ${config.uwsgiPass};"}
        ${optionalString (config.uwsgiPass != null && cfg.uwsgiResolveWhileRunning) ''
          set $nix_proxy_target "${config.uwsgiPass}";
          uwsgi_pass $nix_proxy_target;
        ''}
        ${concatStringsSep "\n" (
          mapAttrsToList (n: v: ''fastcgi_param ${n} "${v}";'') (
            optionalAttrs (config.fastcgiParams != { }) (defaultFastcgiParams // config.fastcgiParams)
          )
        )}
        ${optionalString (config.index != null) "index ${config.index};"}
        ${optionalString (config.tryFiles != null) "try_files ${config.tryFiles};"}
        ${optionalString (config.root != null) "root ${config.root};"}
        ${optionalString (config.alias != null) "alias ${config.alias};"}
        ${optionalString (config.return != null) "return ${toString config.return};"}
        ${config.extraConfig}
        ${optionalString (
          config.proxyPass != null && config.recommendedProxySettings
        ) "include ${recommendedProxyConfig};"}
        ${optionalString (
          config.uwsgiPass != null && config.recommendedUwsgiSettings
        ) "include ${cfg.package}/conf/uwsgi_params;"}
        ${mkBasicAuth "sublocation" config}
      }
    '')
    (sortProperties (mapAttrsToList (k: v: v // { location = k; }) locations))
  );

  mkBasicAuth =
    name: zone:
    optionalString (zone.basicAuthFile != null || zone.basicAuth != { }) (
      let
        auth_file =
          if zone.basicAuthFile != null then zone.basicAuthFile else mkHtpasswd name zone.basicAuth;
      in
      ''
        auth_basic secured;
        auth_basic_user_file ${auth_file};
      ''
    );
  mkHtpasswd =
    name: authDef:
    pkgs.writeText "${name}.htpasswd" (
      concatStringsSep "\n" (
        mapAttrsToList
          (user: password: ''
            ${user}:{PLAIN}${password}
          '')
          authDef
      )
    );


  sanitizeForHeader = s:
    # allow [A-Za-z0-9._~:/@+-] and '/' and '%'; everything else -> '_'
    lib.concatMapStrings
      (c:
        let
          ok =
            (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9")
            || lib.elem c [ "_" "." "-" "/" "@" "+" ":" "~" "%" ];
        in
        if ok then c else "_"
      )
      (lib.stringToCharacters s);
in
concatStringsSep
  "\n"
  (mapAttrsToList
    (vhostName: vhost:
    let
      hostListen =
        if vhost.listen != [ ] then vhost.listen
        else
          [{ addr = "unix:${config.hostenv.upstreamRuntimeDir}/in.sock"; }];

      listenString = { addr, proxyProtocol ? false, extraParameters ? [ ], ... }:
        "
            listen ${addr} "
        + optionalString vhost.default "default_server "
        + optionalString vhost.reuseport "reuseport "
        + optionalString proxyProtocol "proxy_protocol "
        + optionalString (extraParameters != [ ]) (concatStringsSep " " extraParameters)
        + ";";

    in
    ''
      server {
        ${concatMapStringsSep "\n" listenString hostListen}
        server_name ${vhost.serverName} ${concatStringsSep " " vhost.serverAliases};

        ${mkBasicAuth vhostName vhost}

        ${optionalString (vhost.root != null) "root ${vhost.root};"}

        ${optionalString (vhost.globalRedirect != null) ''
          location / {
            return ${toString vhost.redirectCode} https://${vhost.globalRedirect}$request_uri;
          }
        ''}
        ${mkLocations vhost.locations}

        ${vhost.extraConfig}
        ${optionalString enableRouteDebugging "add_header X-Handled $hostenv_handled always;"}
      }
    ''
    )
    virtualHosts
  )
