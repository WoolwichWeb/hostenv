{ ... }:
{
  flake.modules.hostenv.tools-project-upload =
    { pkgs, ... }:
    {
      config.hostenv.projectUploadPackage = pkgs.writeShellApplication {
        name = "hostenv-project-upload";
        runtimeInputs = [ pkgs.rsync ];
        text = ''
          if [ "$#" -ne 2 ]; then
            echo "Usage: hostenv-project-upload SOURCE DESTINATION" >&2
            exit 64
          fi

          exec rsync --delete-after \
            --include '/.git/***' \
            --filter=':- .gitignore' \
            -avz -- "$1" "$2"
        '';
      };
    };
}
