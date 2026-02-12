module Hostenv.Provider.DeployGuidance
    ( trustedKeySetupLines
    ) where

import Data.Text (Text)

trustedKeySetupLines :: Text -> [Text]
trustedKeySetupLines key =
    [ "nix must trust a signing key before nodes accept packages built on this machine."
    , "Add this line in `flake.nix` under `provider = { ... };`:"
    , "  nixSigning.trustedPublicKeys = [ \"" <> key <> "\" ];"
    , "Then run `nix run .#hostenv-provider -- plan` and `nix run .#hostenv-provider -- dns-gate`."
    , "As a workaround for this deployment, rerun with `nix run .#hostenv-provider -- deploy --remote-build`."
    ]
