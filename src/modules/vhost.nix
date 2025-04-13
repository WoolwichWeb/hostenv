# Liberally cribbed from:
# https://github.com/NixOS/nixpkgs/blob/nixos-24.11/nixos/modules/services/web-servers/nginx/default.nix#L298
{ lib, virtualHosts }:

with lib;

let
  mkBasicAuth = name: zone: optionalString (zone.basicAuthFile != null || zone.basicAuth != { }) (
    let
      auth_file =
        if zone.basicAuthFile != null
        then zone.basicAuthFile
        else mkHtpasswd name zone.basicAuth;
    in
    ''
      auth_basic secured;
      auth_basic_user_file ${auth_file};
    ''
  );

  mkHtpasswd = name: authDef: pkgs.writeText "${name}.htpasswd" (
    concatStringsSep "\n" (mapAttrsToList
      (user: password: ''
        ${user}:{PLAIN}${password}
      '')
      authDef)
  );

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

  recommendedProxyConfig = pkgs.writeText "nginx-recommended-proxy-headers.conf" ''
    proxy_set_header        Host $host;
    proxy_set_header        X-Real-IP $remote_addr;
    proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header        X-Forwarded-Proto $scheme;
    proxy_set_header        X-Forwarded-Host $host;
    proxy_set_header        X-Forwarded-Server $host;
  '';

  mkLocations = locations: concatStringsSep "\n" (map
    (config: ''
      location ${config.location} {
        ${optionalString (config.proxyPass != null && !cfg.proxyResolveWhileRunning)
          "proxy_pass ${config.proxyPass};"
        }
        ${optionalString (config.proxyPass != null && cfg.proxyResolveWhileRunning) ''
          set $nix_proxy_target "${config.proxyPass}";
          proxy_pass $nix_proxy_target;
        ''}
        ${optionalString config.proxyWebsockets ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection $connection_upgrade;
        ''}
        ${concatStringsSep "\n"
          (mapAttrsToList (n: v: ''fastcgi_param ${n} "${v}";'')
            (optionalAttrs (config.fastcgiParams != {})
              (defaultFastcgiParams // config.fastcgiParams)))}
        ${optionalString (config.index != null) "index ${config.index};"}
        ${optionalString (config.tryFiles != null) "try_files ${config.tryFiles};"}
        ${optionalString (config.root != null) "root ${config.root};"}
        ${optionalString (config.alias != null) "alias ${config.alias};"}
        ${optionalString (config.return != null) "return ${toString config.return};"}
        ${config.extraConfig}
        ${optionalString (config.proxyPass != null && config.recommendedProxySettings) "include ${recommendedProxyConfig};"}
        ${mkBasicAuth "sublocation" config}
      }
    '')
    (sortProperties (mapAttrsToList (k: v: v // { location = k; }) locations)));
in
concatStringsSep
  "\n"
  (mapAttrsToList
    (vhostName: vhost:
    let
      onlySSL = vhost.onlySSL || vhost.enableSSL;
      hasSSL = onlySSL || vhost.addSSL || vhost.forceSSL;

      # First evaluation of defaultListen based on a set of listen lines.
      mkDefaultListenVhost = listenLines:
        # If this vhost has SSL or is a SSL rejection host.
        # We enable a TLS variant for lines without explicit ssl or ssl = true.
        optionals (hasSSL || vhost.rejectSSL)
          (map (listen: { port = cfg.defaultSSLListenPort; ssl = true; } // listen)
            (filter (listen: !(listen ? ssl) || listen.ssl) listenLines))
        # If this vhost is supposed to serve HTTP
        # We provide listen lines for those without explicit ssl or ssl = false.
        ++ optionals (!onlySSL)
          (map (listen: { port = cfg.defaultHTTPListenPort; ssl = false; } // listen)
            (filter (listen: !(listen ? ssl) || !listen.ssl) listenLines));

      defaultListen =
        if vhost.listen != [ ] then vhost.listen
        else
          if cfg.defaultListen != [ ] then
            mkDefaultListenVhost
              # Cleanup nulls which will mess up with //.
              # TODO: is there a better way to achieve this? i.e. mergeButIgnoreNullPlease?
              (map (listenLine: filterAttrs (_: v: (v != null)) listenLine) cfg.defaultListen)
          else
            let addrs = if vhost.listenAddresses != [ ] then vhost.listenAddresses else cfg.defaultListenAddresses;
            in mkDefaultListenVhost (map (addr: { inherit addr; }) addrs);


      hostListen =
        if vhost.forceSSL
        then filter (x: x.ssl) defaultListen
        else defaultListen;

      listenString = { addr, port, ssl, proxyProtocol ? false, extraParameters ? [ ], ... }:
        # UDP listener for QUIC transport protocol.
        (optionalString (ssl && vhost.quic) ("
            listen ${addr}${optionalString (port != null) ":${toString port}"} quic "
        + optionalString vhost.default "default_server "
        + optionalString vhost.reuseport "reuseport "
        + optionalString (extraParameters != [ ]) (concatStringsSep " "
          (
            let
              inCompatibleParameters = [ "accept_filter" "backlog" "deferred" "fastopen" "http2" "proxy_protocol" "so_keepalive" "ssl" ];
              isCompatibleParameter = param: !(any (p: lib.hasPrefix p param) inCompatibleParameters);
            in
            filter isCompatibleParameter extraParameters
          ))
        + ";"))
        + "
            listen ${addr}${optionalString (port != null) ":${toString port}"} "
        + optionalString (ssl && vhost.http2 && oldHTTP2) "http2 "
        + optionalString ssl "ssl "
        + optionalString vhost.default "default_server "
        + optionalString vhost.reuseport "reuseport "
        + optionalString proxyProtocol "proxy_protocol "
        + optionalString (extraParameters != [ ]) (concatStringsSep " " extraParameters)
        + ";";

      redirectListen = filter (x: !x.ssl) defaultListen;

      # The acme-challenge location doesn't need to be added if we are not using any automated
      # certificate provisioning and can also be omitted when we use a certificate obtained via a DNS-01 challenge
      acmeName = if vhost.useACMEHost != null then vhost.useACMEHost else vhost.serverName;
      acmeLocation = optionalString ((vhost.enableACME || vhost.useACMEHost != null) && config.security.acme.certs.${acmeName}.dnsProvider == null)
        # Rule for legitimate ACME Challenge requests (like /.well-known/acme-challenge/xxxxxxxxx)
        # We use ^~ here, so that we don't check any regexes (which could
        # otherwise easily override this intended match accidentally).
        ''
          location ^~ /.well-known/acme-challenge/ {
            ${optionalString (vhost.acmeFallbackHost != null) "try_files $uri @acme-fallback;"}
            ${optionalString (vhost.acmeRoot != null) "root ${vhost.acmeRoot};"}
            auth_basic off;
            auth_request off;
          }
          ${optionalString (vhost.acmeFallbackHost != null) ''
            location @acme-fallback {
              auth_basic off;
              auth_request off;
              proxy_pass http://${vhost.acmeFallbackHost};
            }
          ''}
        '';

    in
    ''
      ${optionalString vhost.forceSSL ''
        server {
          ${concatMapStringsSep "\n" listenString redirectListen}

          server_name ${vhost.serverName} ${concatStringsSep " " vhost.serverAliases};

          location / {
            return ${toString vhost.redirectCode} https://$host$request_uri;
          }
          ${acmeLocation}
        }
      ''}

      server {
        ${concatMapStringsSep "\n" listenString hostListen}
        server_name ${vhost.serverName} ${concatStringsSep " " vhost.serverAliases};
        ${optionalString (hasSSL && vhost.http2 && !oldHTTP2) ''
          http2 on;
        ''}
        ${optionalString (hasSSL && vhost.quic) ''
          http3 ${if vhost.http3 then "on" else "off"};
          http3_hq ${if vhost.http3_hq then "on" else "off"};
        ''}
        ${optionalString hasSSL ''
          ssl_certificate ${vhost.sslCertificate};
          ssl_certificate_key ${vhost.sslCertificateKey};
        ''}
        ${optionalString (hasSSL && vhost.sslTrustedCertificate != null) ''
          ssl_trusted_certificate ${vhost.sslTrustedCertificate};
        ''}
        ${optionalString vhost.rejectSSL ''
          ssl_reject_handshake on;
        ''}
        ${optionalString (hasSSL && vhost.kTLS) ''
          ssl_conf_command Options KTLS;
        ''}

        ${mkBasicAuth vhostName vhost}

        ${optionalString (vhost.root != null) "root ${vhost.root};"}

        ${optionalString (vhost.globalRedirect != null) ''
          location / {
            return ${toString vhost.redirectCode} http${optionalString hasSSL "s"}://${vhost.globalRedirect}$request_uri;
          }
        ''}
        ${acmeLocation}
        ${mkLocations vhost.locations}

        ${vhost.extraConfig}
      }
    ''
    )
    virtualHosts
  )
