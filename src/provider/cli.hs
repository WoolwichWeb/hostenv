#!/usr/bin/env -S runghc
{-# LANGUAGE OverloadedStrings #-}

-- hostenv-provider CLI: plan | deploy
-- This CLI stays provider-neutral; DNS/Cloudflare logic has been removed.

import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Conversions (convertText)
import Options.Applicative qualified as OA
import Turtle qualified as Sh
import System.Exit (ExitCode(..))
import Prelude hiding (FilePath)

-- Commands
 data Command = CmdPlan | CmdDeploy { cNode :: Maybe Text }

data CLI = CLI { cliDest :: Text, cliCmd :: Command }

destOpt :: OA.Parser Text
destOpt =
  T.pack <$> OA.strOption
    ( OA.long "dest"
        <> OA.metavar "DIR"
        <> OA.value "generated"
        <> OA.showDefault
        <> OA.help "Output directory for generated plan/state/flake"
    )

nodeOpt :: OA.Parser (Maybe Text)
nodeOpt = OA.optional . fmap T.pack $
  OA.strOption
    ( OA.long "node"
        <> OA.short 'n'
        <> OA.metavar "NODE"
        <> OA.help "Restrict to a specific node"
    )

cliParser :: OA.Parser CLI
cliParser =
  CLI
    <$> destOpt
    <*> OA.hsubparser
      ( OA.command "plan" (OA.info (pure CmdPlan) (OA.progDesc "Generate plan/state/flake"))
          <> OA.command "deploy" (OA.info (CmdDeploy <$> nodeOpt) (OA.progDesc "Deploy via deploy-rs"))
      )

cliOpts :: OA.ParserInfo CLI
cliOpts =
  OA.info
    (cliParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.header "hostenv-provider"
    )

-- Run plan by invoking hostenv-provider-plan script that was built in the flake
runPlan :: Text -> IO ()
runPlan dest = do
  Sh.shells ("HOSTENV_PROVIDER_OUT=" <> convertText dest <> " hostenv-provider-plan") Sh.empty >>= \case
    ExitSuccess -> pure ()
    ec -> Sh.die (convertText ("plan failed with exit code " <> T.pack (show ec)))

-- Deploy wrapper: call deploy-rs via the generated flake in DEST
runDeploy :: Text -> Maybe Text -> IO ()
runDeploy dest mNode = do
  let baseArgs = ["--extra-experimental-features", "nix-command flakes", "run", dest <> "#deploy"]
      args = maybe baseArgs (\n -> baseArgs ++ ["-n", convertText n]) mNode
  Sh.shells (T.unwords args) Sh.empty >>= \case
    ExitSuccess -> pure ()
    ec -> Sh.die (convertText ("deploy failed with exit code " <> T.pack (show ec)))

main :: IO ()
main = do
  opts <- OA.execParser cliOpts
  let dest = cliDest opts
  case cliCmd opts of
    CmdPlan -> runPlan dest
    CmdDeploy node -> runDeploy dest node
