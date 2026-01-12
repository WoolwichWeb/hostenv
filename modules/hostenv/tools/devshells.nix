{ ... }:
{
  flake.modules.hostenv.tools-devshells =
    { lib, config, pkgs, ... }:
    let
      subCommandList = lib.attrsToList config.hostenv.subCommands;
      scripts = builtins.filter (cmd: cmd.value.makeScript) subCommandList;

      hostenvShells =
        lib.mapAttrs
          (environmentName: environment:
            let
              scriptDerivations = builtins.map
                (cmd: pkgs.writeShellScriptBin cmd.name ''
                  exec hostenv ${cmd.name} -- "$@"
                '')
                scripts;
              envStartup = ''
                currentBranch=$(git symbolic-ref --short HEAD)

                if [ ! "$currentBranch" = "${environmentName}" ]; then
                  if git show-ref --quiet --branches "${environmentName}"; then
                    git checkout "${environmentName}" || exit 1
                  fi
                fi

                hostenv banner
              '';
            in
            {
              devshell = {
                name = "hostenv-${environmentName}";
                packages = with pkgs; [
                  config.hostenv.cliPackage
                  restic
                  boxes
                  jq
                ] ++ scriptDerivations;
                startup.hostenv = { text = envStartup; };
              };
              env = [
                {
                  name = "DEVSHELL_NO_MOTD";
                  value = 1;
                }
              ];
            })
          (lib.filterAttrs (_: v: v.enable) config.hostenv.publicEnvironments)
        // {
          default = {
            devshell = {
              name = "hostenv";
              startup.hostenv = {
                text = ''
                  ENV_JSON='${builtins.toJSON (lib.filterAttrs (_: v: v.enable) config.hostenv.publicEnvironments)}'

                  if gitRef=$(git symbolic-ref -q --short HEAD 2>/dev/null); then
                    # Is current branch a hostenv environment?
                    if jq -e --arg ref "$gitRef" 'has($ref)' <<<"$ENV_JSON" >/dev/null; then
                      : # OK
                    else
                      printf '%s\n\n%s\n' \
                "⚠️  Cannot load hostenv — '$gitRef' is not a hostenv environment" \
                "ℹ️  Add 'environments.$gitRef.enable = true;' to your hostenv.nix if you would like to make it a hostenv environment."
                    fi
                  else
                    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
                      echo "⚠️  Cannot load hostenv — git is in detached HEAD state"
                    else
                      echo "⚠️  Cannot load hostenv — this is not a git repository"
                    fi
                  fi
                '';
              };
            };
            env = [
              {
                name = "DEVSHELL_NO_MOTD";
                value = 1;
              }
            ];
          };
        };
    in
    {
      config.hostenv.devShells = lib.mkDefault hostenvShells;
    };
}
