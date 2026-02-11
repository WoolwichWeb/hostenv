module Hostenv.Provider.DeployPreflight
    ( DeployPreflightConfig (..)
    , PreflightFailure (..)
    , runPreflight
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))

data DeployPreflightConfig = DeployPreflightConfig
    { nodeName :: Text
    , deployUser :: Text
    , requiresTrustedKey :: Bool
    , signingPublicKey :: Maybe Text
    , runRemote :: [Text] -> IO (ExitCode, Text, Text)
    }

data PreflightFailure = PreflightFailure
    { failureNode :: Text
    , failureReason :: Text
    , failureRemediation :: [Text]
    }

runPreflight :: DeployPreflightConfig -> IO (Maybe PreflightFailure)
runPreflight cfg = do
    nixOsRes <- checkNixOs cfg
    case nixOsRes of
        Left reason ->
            pure . Just $
                PreflightFailure
                    { failureNode = cfg.nodeName
                    , failureReason = reason
                    , failureRemediation =
                        [ "Verify SSH connectivity and run `cat /etc/os-release` manually."
                        ]
                    }
        Right False ->
            pure . Just $
                PreflightFailure
                    { failureNode = cfg.nodeName
                    , failureReason = "target machine is not NixOS"
                    , failureRemediation =
                        [ "Provision this node with NixOS before deploying with hostenv."
                        ]
                    }
        Right True ->
            if not cfg.requiresTrustedKey
                then pure Nothing
                else case cfg.signingPublicKey of
                    Nothing ->
                        pure . Just $
                            PreflightFailure
                                { failureNode = cfg.nodeName
                                , failureReason = "internal error: signing public key missing during trust check"
                                , failureRemediation =
                                    [ "Re-run with `--signing-key-file` or set `HOSTENV_SIGNING_KEY_FILE`."
                                    ]
                                }
                    Just key -> do
                        trustedKeysRes <- readTrustedPublicKeys cfg
                        case trustedKeysRes of
                            Left reason ->
                                pure . Just $
                                    PreflightFailure
                                        { failureNode = cfg.nodeName
                                        , failureReason = reason
                                        , failureRemediation =
                                            [ "Ensure user `" <> cfg.deployUser <> "` can run `nix config show trusted-public-keys` (with or without sudo)."
                                            ]
                                        }
                            Right trustedKeys ->
                                if key `elem` trustedKeys
                                    then pure Nothing
                                    else
                                        pure . Just $
                                            PreflightFailure
                                                { failureNode = cfg.nodeName
                                                , failureReason = "signing key is not trusted by remote Nix daemon"
                                                , failureRemediation =
                                                    [ "Add the key to provider config: `provider.nixSigning.trustedPublicKeys = [ \"" <> key <> "\" ];`."
                                                    , "Apply the updated NixOS configuration on this node, regenerate `generated/plan.json`, and retry deploy."
                                                    ]
                                                }

checkNixOs :: DeployPreflightConfig -> IO (Either Text Bool)
checkNixOs cfg = do
    let cmd = ["cat", "/etc/os-release"]
    (code, out, err) <- cfg.runRemote cmd
    case code of
        ExitSuccess ->
            let lines' = map T.strip (T.lines out)
                idValue = lookupOsReleaseKey "ID" lines'
             in pure (Right (idValue == Just "nixos"))
        ExitFailure _ ->
            pure (Left (commandFailure "unable to read /etc/os-release" cmd code out err))

lookupOsReleaseKey :: Text -> [Text] -> Maybe Text
lookupOsReleaseKey key lines' =
    let keyPrefix = key <> "="
        parseLine line =
            if keyPrefix `T.isPrefixOf` line
                then Just (stripOptionalQuotes (T.drop (T.length keyPrefix) line))
                else Nothing
     in foldr (<|>) Nothing (map parseLine lines')
  where
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

stripOptionalQuotes :: Text -> Text
stripOptionalQuotes value =
    let stripped = T.strip value
     in case (T.uncons stripped, T.unsnoc stripped) of
            (Just ('"', _), Just (_, '"')) -> T.drop 1 (T.dropEnd 1 stripped)
            (Just ('\'', _), Just (_, '\'')) -> T.drop 1 (T.dropEnd 1 stripped)
            _ -> stripped

readTrustedPublicKeys :: DeployPreflightConfig -> IO (Either Text [Text])
readTrustedPublicKeys cfg = do
    let sudoCmd = ["sudo", "nix", "config", "show", "trusted-public-keys"]
    (sudoCode, sudoOut, sudoErr) <- cfg.runRemote sudoCmd
    case sudoCode of
        ExitSuccess ->
            pure (Right (parseTrustedPublicKeys sudoOut))
        ExitFailure _ -> do
            let userCmd = ["nix", "config", "show", "trusted-public-keys"]
            (userCode, userOut, userErr) <- cfg.runRemote userCmd
            case userCode of
                ExitSuccess ->
                    pure (Right (parseTrustedPublicKeys userOut))
                ExitFailure _ ->
                    pure
                        ( Left $
                            T.intercalate
                                "\n"
                                [ commandFailure "reading trusted-public-keys via sudo failed" sudoCmd sudoCode sudoOut sudoErr
                                , commandFailure "reading trusted-public-keys without sudo failed" userCmd userCode userOut userErr
                                ]
                        )

parseTrustedPublicKeys :: Text -> [Text]
parseTrustedPublicKeys output =
    let keys = filter (not . T.null) (T.words (T.strip output))
     in dedupeKeepOrder keys

dedupeKeepOrder :: [Text] -> [Text]
dedupeKeepOrder values = reverse (fst (foldl step ([], []) values))
  where
    step (acc, seen) value =
        if value `elem` seen
            then (acc, seen)
            else (value : acc, value : seen)

showCmd :: [Text] -> Text
showCmd = T.unwords . map shellEscape

trimmedOutputFallback :: Text -> Text -> Text
trimmedOutputFallback out err =
    let out' = T.strip out
        err' = T.strip err
     in if T.null err' then out' else err'

commandFailure :: Text -> [Text] -> ExitCode -> Text -> Text -> Text
commandFailure context args code out err =
    let detail = trimmedOutputFallback out err
     in context
            <> ": "
            <> showCmd args
            <> " (exit "
            <> T.pack (show code)
            <> ")"
            <> if T.null detail then "" else " - " <> detail

shellEscape :: Text -> Text
shellEscape t =
    if T.null t
        then "''"
        else "'" <> T.replace "'" "'\\''" t <> "'"
