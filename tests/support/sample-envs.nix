{ lib }:
let
  mkProviderEnv = { name, userName ? null, enable ? true, publicKeys ? [], type ? "development" }:
    {
      inherit enable type;
      users = {
        ${name} = {
          email = "user@example.test";
          publicKeys = publicKeys;
        };
      };
      virtualHosts = { };
      hostenv = {
        hostname = name;
        userName = userName;
      };
    };
  mkSingle = args@{ name, ... }: { ${name} = mkProviderEnv args; };
in {
  inherit mkProviderEnv mkSingle;
}
