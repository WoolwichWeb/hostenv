{ ... }:
let
  serviceSrc = builtins.path {
    path = ./.;
    name = "hostenv-provider-service-src";
  };
  haskellDeps = [
    "aeson"
    "bytestring"
    "cookie"
    "cryptonite"
    "http-client"
    "http-client-tls"
    "http-types"
    "lucid"
    "memory"
    "network"
    "postgresql-simple"
    "servant"
    "servant-server"
    "text"
    "tagged"
    "time"
    "turtle"
    "unix"
    "wai"
    "warp"
  ];
in
{
  config.flake.lib.provider.service = {
    src = serviceSrc;
    inherit haskellDeps;
  };
}
