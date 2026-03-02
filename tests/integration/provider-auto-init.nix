{ pkgs, inputs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  system = pkgs.stdenv.hostPlatform.system;
  modules = inputs.import-tree ../../modules;
  moduleList = if builtins.isList modules then modules else [ modules ];

  mkProviderFlake = autoInit:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ system ];
      imports = [ inputs.devshell.flakeModule ] ++ moduleList;

      project.enable = false;
      provider = {
        enable = true;
        hostenvHostname = "hosting.test";
        nodeFor = { default = "node1"; };
        nodeSystems = { node1 = system; };
        plan.autoInit = autoInit;
      };
    };

  findAutoInitScript = flake:
    let
      startup = flake.allSystems.${system}.config.devshells.default.devshell.startup or { };
      names = builtins.attrNames startup;
      matches =
        lib.filter
          (name:
            let text = startup.${name}.text or "";
            in
            lib.strings.hasInfix "secrets/provider.yaml" text
            && lib.strings.hasInfix "generated/state.json" text
          )
          names;
    in
    if matches == [ ] then "" else startup.${lib.head matches}.text;

  autoInitEnabledFlake = mkProviderFlake true;
  autoInitDisabledFlake = mkProviderFlake false;

  autoInitEnabledScript = findAutoInitScript autoInitEnabledFlake;
  autoInitDisabledScript = findAutoInitScript autoInitDisabledFlake;

  autoInitEnabledRun = pkgs.runCommand "provider-auto-init-enabled" {
    buildInputs = [ pkgs.bash pkgs.coreutils pkgs.findutils pkgs.gnugrep pkgs.gnused pkgs.sops pkgs.age pkgs.yq-go ];
  } ''
    set -euo pipefail

    workdir="$TMPDIR/provider-auto-init-enabled"
    mkdir -p "$workdir"
    cd "$workdir"

    ${autoInitEnabledScript}

    test -f secrets/provider.yaml
    test -f generated/state.json
    test -f secrets/.sops.yaml
    test "$(cat generated/state.json)" = "{}"

    key_file=""
    for candidate in secrets/*; do
      if [ -f "$candidate" ] && grep -q '^AGE-SECRET-KEY-' "$candidate"; then
        key_file="$candidate"
        break
      fi
    done

    test -n "$key_file"

    recipient="$(age-keygen -y "$key_file")"
    grep -q "$recipient" secrets/.sops.yaml

    ${pkgs.sops}/bin/sops --decrypt --output-type yaml secrets/provider.yaml >/dev/null

    echo ok > "$out"
  '';

  autoInitDisabledRun = pkgs.runCommand "provider-auto-init-disabled" {
    buildInputs = [ pkgs.bash pkgs.coreutils pkgs.findutils pkgs.gnugrep pkgs.gnused pkgs.sops pkgs.age pkgs.yq-go ];
  } ''
    set -euo pipefail

    workdir="$TMPDIR/provider-auto-init-disabled"
    mkdir -p "$workdir"
    cd "$workdir"

    ${autoInitDisabledScript}

    test ! -e secrets/provider.yaml
    test ! -e generated/state.json
    test ! -e secrets/.sops.yaml

    echo ok > "$out"
  '';
in
{
  provider-auto-init-script-present =
    asserts.assertTrue "provider-auto-init-script-present"
      (autoInitEnabledScript != "")
      "provider.plan.autoInit should install a devshell startup script";

  provider-auto-init-generates-files = autoInitEnabledRun;

  provider-auto-init-can-disable =
    asserts.assertTrue "provider-auto-init-can-disable"
      (autoInitDisabledScript == "")
      "provider.plan.autoInit = false should disable the auto-init startup script";

  provider-auto-init-disabled-no-files = autoInitDisabledRun;
}
