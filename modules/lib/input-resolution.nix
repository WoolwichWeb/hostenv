{ ... }:
let
  hostenvInputs =
    let
      resolveHostenvInput = { inputs, context ? "hostenv" }:
        if inputs ? hostenv
        then inputs.hostenv
        else if inputs ? self
        then inputs.self
        else
          builtins.throw ''
            ${context}: missing hostenv input.

            Expected either:
            - inputs.hostenv (downstream provider/project flakes)
            - or inputs.self (hostenv itself)
          '';

      requireInput = { inputs, name, context ? "hostenv" }:
        if builtins.hasAttr name inputs then
          inputs.${name}
        else if name == "hostenv" then
          resolveHostenvInput { inherit inputs context; }
        else
          let
            hostenvInput =
              if inputs ? hostenv then inputs.hostenv
              else if inputs ? self then inputs.self
              else null;
          in
          if hostenvInput != null
            && hostenvInput ? inputs
            && builtins.hasAttr name hostenvInput.inputs
          then
            hostenvInput.inputs.${name}
          else
            builtins.throw ''
              ${context}: missing required flake input '${name}'.

              Looked in:
              - inputs.${name}
              - inputs.hostenv.inputs.${name} (or inputs.self.inputs.${name})
            '';
    in
    {
      inherit resolveHostenvInput requireInput;
    };
in
{
  config.flake.lib.hostenvInputs = hostenvInputs;
}
