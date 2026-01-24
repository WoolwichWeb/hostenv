{ ... }:
let
  locationOptions =
    { lib, config, ... }:
      with lib;
      {
        options = {
          basicAuth = mkOption {
            type = types.attrsOf types.str;
            default = { };
            example = literalExpression ''
              {
                user = "password";
              };
            '';
            description = ''
              Basic Auth protection for a vhost.

              WARNING: This is implemented to store the password in plain text in the
              Nix store.
            '';
          };

          basicAuthFile = mkOption {
            type = types.nullOr types.path;
            default = null;
            description = ''
              Basic Auth password file for a vhost.
              Can be created by running {command}`nix-shell --packages apacheHttpd --run 'htpasswd -B -c FILENAME USERNAME'`.
            '';
          };

          proxyPass = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "http://www.example.org/";
            description = ''
              Adds proxy_pass directive and sets recommended proxy headers if
              recommendedProxySettings is enabled.
            '';
          };

          proxyWebsockets = mkOption {
            type = types.bool;
            default = false;
            example = true;
            description = ''
              Whether to support proxying websocket connections with HTTP/1.1.
            '';
          };

          uwsgiPass = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "unix:/run/example/example.sock";
            description = ''
              Adds uwsgi_pass directive and sets recommended proxy headers if
              recommendedUwsgiSettings is enabled.
            '';
          };

          index = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "index.php index.html";
            description = ''
              Adds index directive.
            '';
          };

          tryFiles = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "$uri =404";
            description = ''
              Adds try_files directive.
            '';
          };

          root = mkOption {
            type = types.nullOr types.path;
            default = null;
            example = "/your/root/directory";
            description = ''
              Root directory for requests.
            '';
          };

          alias = mkOption {
            type = types.nullOr types.path;
            default = null;
            example = "/your/alias/directory";
            description = ''
              Alias directory for requests.
            '';
          };

          return = mkOption {
            type =
              with types;
              nullOr (oneOf [
                str
                int
              ]);
            default = null;
            example = "301 http://example.com$request_uri";
            description = ''
              Adds a return directive, for e.g. redirections.
            '';
          };

          fastcgiParams = mkOption {
            type = types.attrsOf (types.either types.str types.path);
            default = { };
            description = ''
              FastCGI parameters to override.  Unlike in the Nginx
              configuration file, overriding only some default parameters
              won't unset the default values for other parameters.
            '';
          };

          extraConfig = mkOption {
            type = types.lines;
            default = "";
            description = ''
              These lines go to the end of the location verbatim.
            '';
          };

          priority = mkOption {
            type = types.int;
            default = 1000;
            description = ''
              Order of this location block in relation to the others in the vhost.
              The semantics are the same as with `lib.mkOrder`. Smaller values have
              a greater priority.
            '';
          };

          recommendedProxySettings = mkOption {
            type = types.bool;
            default = config.services.nginx.recommendedProxySettings;
            defaultText = literalExpression "config.services.nginx.recommendedProxySettings";
            description = ''
              Enable recommended proxy settings.
            '';
          };

          recommendedUwsgiSettings = mkOption {
            type = types.bool;
            default = config.services.nginx.recommendedUwsgiSettings;
            defaultText = literalExpression "config.services.nginx.recommendedUwsgiSettings";
            description = ''
              Enable recommended uwsgi settings.
            '';
          };
        };
      };

  vhostOptions =
    { lib, config, pkgs, ... }:
      with lib;
      {
        options = {
          serverName = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Name of this virtual host. Defaults to attribute name in virtualHosts.
            '';
            example = "example.org";
          };

          serverAliases = mkOption {
            type = types.listOf types.str;
            default = [ ];
            example = [
              "www.example.org"
              "example.org"
            ];
            description = ''
              Additional names of virtual hosts served by this virtual host configuration.
            '';
          };

          listen = mkOption {
            type =
              with types;
              listOf (submodule {
                options = {
                  addr = mkOption {
                    type = str;
                    description = "Listen address.";
                  };
                  port = mkOption {
                    type = types.nullOr port;
                    description = ''
                      Port number to listen on.
                      If unset and the listen address is not a socket then nginx defaults to 80.
                    '';
                    default = null;
                  };
                  ssl = mkOption {
                    type = bool;
                    description = "Enable SSL.";
                    default = false;
                  };
                  proxyProtocol = mkOption {
                    type = bool;
                    description = "Enable PROXY protocol.";
                    default = false;
                  };
                  extraParameters = mkOption {
                    type = listOf str;
                    description = "Extra parameters of this listen directive.";
                    default = [ ];
                    example = [
                      "backlog=1024"
                      "deferred"
                    ];
                  };
                };
              });
            default = [ ];
            example = [
              {
                addr = "195.154.1.1";
                port = 443;
                ssl = true;
              }
              {
                addr = "192.154.1.1";
                port = 80;
              }
              { addr = "unix:/var/run/nginx.sock"; }
            ];
            description = ''
              Listen addresses and ports for this virtual host.
              IPv6 addresses must be enclosed in square brackets.
              Note: this option overrides `addSSL`
              and `onlySSL`.

              If you only want to set the addresses manually and not
              the ports, take a look at `listenAddresses`.
            '';
          };

          listenAddresses = mkOption {
            type = with types; listOf str;

            description = ''
              Listen addresses for this virtual host.
              Compared to `listen` this only sets the addresses
              and the ports are chosen automatically.

              Note: This option overrides `enableIPv6`
            '';
            default = [ ];
            example = [
              "127.0.0.1"
              "[::1]"
            ];
          };

          addSSL = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to enable HTTPS in addition to plain HTTP. This will set defaults for
              `listen` to listen on all interfaces on the respective default
              ports (80, 443).
            '';
          };

          onlySSL = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to enable HTTPS and reject plain HTTP connections. This will set
              defaults for `listen` to listen on all interfaces on port 443.
            '';
          };

          forceSSL = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to add a separate nginx server block that redirects (defaults
              to 301, configurable with `redirectCode`) all plain HTTP traffic to
              HTTPS. This will set defaults for `listen` to listen on all interfaces
              on the respective default ports (80, 443), where the non-SSL listens
              are used for the redirect vhosts.
            '';
          };

          rejectSSL = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to listen for and reject all HTTPS connections to this vhost. Useful in
              [default](#opt-services.nginx.virtualHosts._name_.default)
              server blocks to avoid serving the certificate for another vhost. Uses the
              `ssl_reject_handshake` directive available in nginx versions
              1.19.4 and above.
            '';
          };

          kTLS = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to enable kTLS support.
              Implementing TLS in the kernel (kTLS) improves performance by significantly
              reducing the need for copying operations between user space and the kernel.
              Required Nginx version 1.21.4 or later.
            '';
          };

          sslCertificate = mkOption {
            type = types.path;
            example = "/var/host.cert";
            description = "Path to server SSL certificate.";
          };

          sslCertificateKey = mkOption {
            type = types.path;
            example = "/var/host.key";
            description = "Path to server SSL certificate key.";
          };

          sslTrustedCertificate = mkOption {
            type = types.nullOr types.path;
            default = null;
            example = literalExpression ''"''${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"'';
            description = "Path to root SSL certificate for stapling and client certificates.";
          };

          http2 = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Whether to enable the HTTP/2 protocol.
              Note that (as of writing) due to nginx's implementation, to disable
              HTTP/2 you have to disable it on all vhosts that use a given
              IP address / port.
              If there is one server block configured to enable http2, then it is
              enabled for all server blocks on this IP.
              See <https://stackoverflow.com/a/39466948/263061>.
            '';
          };

          http3 = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Whether to enable the HTTP/3 protocol.
              This requires using `pkgs.nginxQuic` package
              which can be achieved by setting `services.nginx.package = pkgs.nginxQuic;`
              and activate the QUIC transport protocol
              `services.nginx.virtualHosts.<name>.quic = true;`.
              Note that HTTP/3 support is experimental and *not* yet recommended for production.
              Read more at <https://quic.nginx.org/>
              HTTP/3 availability must be manually advertised, preferably in each location block.
            '';
          };

          http3_hq = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to enable the HTTP/0.9 protocol negotiation used in QUIC interoperability tests.
              This requires using `pkgs.nginxQuic` package
              which can be achieved by setting `services.nginx.package = pkgs.nginxQuic;`
              and activate the QUIC transport protocol
              `services.nginx.virtualHosts.<name>.quic = true;`.
              Note that special application protocol support is experimental and *not* yet recommended for production.
              Read more at <https://quic.nginx.org/>
            '';
          };

          quic = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether to enable the QUIC transport protocol.
              This requires using `pkgs.nginxQuic` package
              which can be achieved by setting `services.nginx.package = pkgs.nginxQuic;`.
              Note that QUIC support is experimental and
              *not* yet recommended for production.
              Read more at <https://quic.nginx.org/>
            '';
          };

          reuseport = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Create an individual listening socket .
              It is required to specify only once on one of the hosts.
            '';
          };

          root = mkOption {
            type = types.nullOr types.path;
            default = null;
            example = "/data/webserver/docs";
            description = ''
              The path of the web root directory.
            '';
          };

          default = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Makes this vhost the default.
            '';
          };

          extraConfig = mkOption {
            type = types.lines;
            default = "";
            description = ''
              These lines go to the end of the vhost verbatim.
            '';
          };

          globalRedirect = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "newserver.example.org";
            description = ''
              If set, all requests for this host are redirected (defaults to 301,
              configurable with `redirectCode`) to the given hostname.
            '';
          };

          redirectCode = mkOption {
            type = types.ints.between 300 399;
            default = 301;
            example = 308;
            description = ''
              HTTP status used by `globalRedirect` and `forceSSL`. Possible usecases
              include temporary (302, 307) redirects, keeping the request method and
              body (307, 308), or explicitly resetting the method to GET (303).
              See <https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections>.
            '';
          };

          basicAuth = mkOption {
            type = types.attrsOf types.str;
            default = { };
            example = literalExpression ''
              {
                user = "password";
              };
            '';
            description = ''
              Basic Auth protection for a vhost.

              WARNING: This is implemented to store the password in plain text in the
              Nix store.
            '';
          };

          basicAuthFile = mkOption {
            type = types.nullOr types.path;
            default = null;
            description = ''
              Basic Auth password file for a vhost.
              Can be created by running {command}`nix-shell --packages apacheHttpd --run 'htpasswd -B -c FILENAME USERNAME'`.
            '';
          };

          locations = mkOption {
            type = types.attrsOf (
              types.submodule (
                locationOptions { inherit lib config; }
              )
            );
            default = { };
            example = literalExpression ''
              {
                "/" = {
                  proxyPass = "http://localhost:3000";
                };
              };
            '';
            description = "Declarative location config";
          };
        };
      };

  vhostValues =
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
      );
in
{
  flake.lib.hostenv.nginx = {
    inherit locationOptions vhostOptions vhostValues;
  };
}
