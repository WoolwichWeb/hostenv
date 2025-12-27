# shellcheck shell=bash

declare -g composerVendor

hostenvRestoreInstallerPaths() {
  if [[ -z "${composerVendor:-}" || ! -d "${composerVendor}" ]]; then
    return 0
  fi
  if [[ ! -f composer.json ]]; then
    return 0
  fi

  mapfile -t installer_paths < <(jq -r -c 'try((.extra."installer-paths") | keys[])' composer.json)
  for installer_path in "${installer_paths[@]}"; do
    # Remove everything after {$name} placeholder
    installer_path="${installer_path/\{\$name\}*/}"
    src_path="${composerVendor}/${installer_path/\{\$name\}*/}"
    if [[ -d "$src_path" ]]; then
      mkdir -p "$(dirname "$installer_path")"
      cp -ar "$src_path" "$installer_path"
      chmod -R +w "$installer_path" || true
    fi
  done
}

if declare -f composerInstallBuildHook >/dev/null; then
  eval "$(declare -f composerInstallBuildHook | sed '1s/composerInstallBuildHook/hostenvComposerInstallBuildHook/')"
  composerInstallBuildHook() {
    hostenvRestoreInstallerPaths
    hostenvComposerInstallBuildHook
  }
else
  composerInstallBuildHook() {
    hostenvRestoreInstallerPaths
  }
fi
