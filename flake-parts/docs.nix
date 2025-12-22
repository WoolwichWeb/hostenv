{ ... }:
{
  perSystem = { pkgs, self', config, ... }:
    let
      docSearch = pkgs.writeTextDir "index.html" "<html><body>docs disabled</body></html>";
      serveDocs = pkgs.writeShellApplication {
        name = "serve-docs";
        runtimeInputs = [ pkgs.http-server ];
        runtimeEnv.server_flags = [ "--port=0" "-c-1" "-o" ];
        text = ''
          http-server ${docSearch} "''${server_flags[@]}"
        '';
      };
    in
    {
      apps.default = {
        type = "app";
        program = "${serveDocs}/bin/serve-docs";
        meta.description = "Serve hostenv documentation site";
      };
      packages = {
        inherit docSearch;
        default = config.packages.hostenv-provider;
      };
    };
}
