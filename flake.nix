{
  description = "Hostenv: the PaaS you control";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    search = {
      url = "github:NuschtOS/search";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { nixpkgs, flake-utils, search, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = (import nixpkgs) {
            inherit system;
          };

          docSearch = search.packages.${system}.mkMultiSearch {
            title = "hostenv options search";
            scopes = [
              {
                modules = [
                  ./src/modules/top-level/full-env.nix
                  { _module.args = { inherit pkgs; }; }
                  ({ config, ... }: {
                    hostenv = {
                      organisation = "org";
                      project = "example";
                      root = ./.;
                      environmentName = "main";
                    };
                  })
                  {
                    environments.main = {
                      enable = true;
                      type = "production";
                      virtualHosts = {
                        "www.example.com" = { };
                      };
                    };
                  }
                ];
                urlPrefix = "https://gitlab.com/woolwichweb/hostenv/-/blob/main/";
              }
            ];
          };

          serveDocs = pkgs.writeShellApplication {
            name = "serve-docs";
            runtimeInputs = [ pkgs.http-server ];
            runtimeEnv.server_flags = [
              # Search for available port
              "--port=0"

              # Disable browser cache
              "-c-1"

              # Open using xdg-open
              "-o"
            ];
            text = ''
              http-server ${docSearch} "''${server_flags[@]}"
            '';
          };
        in
        {
          apps.default = flake-utils.lib.mkApp { drv = serveDocs; };
          packages = {
            # Searchable documentation package.
            inherit docSearch;
          };
        }) //
    {
      templates = {
        default = {
          path = ./template;
          welcomeText = ''
            ## Thank you for using Hostenv

            What's next:

             - If you haven't already, install Nix:
               https://nixos.org/download/#download-nix
             - Configure your project in `.hostenv/hostenv.nix`.
             - Check the project README.md for more instructions:
               https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md?ref_type=heads
          '';
        };
      };
    };
}
