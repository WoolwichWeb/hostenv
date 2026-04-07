{ ... }:
let
  serviceSrc = builtins.path {
    path = ./.;
    name = "hostenv-provider-service-src";
  };
  haskellDeps = [
    "addressable-content"
    "aeson"
    "bytestring"
    "containers"
    "cookie"
    "crypton"
    "http-client"
    "http-client-tls"
    "http-types"
    "lucid"
    "memory"
    "network"
    "postgresql-simple"
    "process"
    "servant"
    "servant-server"
    "text"
    "tagged"
    "time"
    "stm"
    "turtle"
    "unix"
    "wai"
    "yaml"
    "wai-websockets"
    "warp"
    "websockets"
  ];
in
{
  config.flake.lib.provider.service = {
    src = serviceSrc;
    inherit haskellDeps;
  };
}
