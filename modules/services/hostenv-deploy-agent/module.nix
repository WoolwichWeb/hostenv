{ ... }:
let
  agentSrc = builtins.path {
    path = ./.;
    name = "hostenv-deploy-agent-src";
  };
  haskellDeps = [
    "async"
    "aeson"
    "bytestring"
    "containers"
    "crypton"
    "directory"
    "filepath"
    "http-client"
    "http-client-tls"
    "http-types"
    "memory"
    "network-uri"
    "process"
    "text"
    "time"
    "unix"
    "websockets"
    "wuss"
  ];
in
{
  config.flake.lib.provider.deployAgent = {
    src = agentSrc;
    inherit haskellDeps;
  };
}
