{ pkgs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };
  templates = {
    provider = import ../../template/provider/flake.nix;
    managed = import (pkgs.writeText "managed-provider-template.nix" (
      builtins.replaceStrings [ "{{HOSTENV_PROJECT_INPUTS}}" ] [ "" ]
        (builtins.readFile ../../template/provider/flake.template.nix)
    ));
  };
  # Capture the template's configuration without evaluating a full provider
  # or fetching its example project inputs.
  templateInputs = {
    flake-parts.lib.mkFlake = _: module: module;
    hostenv.flakeModules.provider = { };
  };
  requiredInputs = [ "hostenv" "nixpkgs" "flake-parts" "phps" "deploy-rs" "sops-nix" ];
in
lib.mapAttrs'
  (name: template:
    let
      provider = (template.outputs templateInputs).provider;
      nodes = lib.unique (builtins.attrValues provider.nodeFor);
      validNodes = nodes != [ ] && lib.all
        (node: builtins.isString node && builtins.hasAttr node provider.nodeSystems)
        nodes;
      hasDeploymentInputs = lib.all
        (input: builtins.hasAttr input template.inputs)
        requiredInputs;
    in
    {
      name = "provider-template-${name}";
      value = asserts.assertTrue "provider-template-${name}"
        (validNodes && hasDeploymentInputs)
        "provider templates must declare a system for every selected node and all generated-flake parent inputs";
    })
  templates
