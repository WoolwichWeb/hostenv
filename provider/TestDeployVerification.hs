{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Hostenv.Provider.DeployVerification
import System.Exit (exitFailure)
import System.Exit qualified as Exit

assert :: Bool -> Text -> IO ()
assert cond msg =
  unless cond $ do
    putStrLn ("FAIL: " <> show msg)
    exitFailure

mkRequest :: VerificationRequest
mkRequest =
  VerificationRequest
    { vrVirtualHost = "www.example.test"
    , vrPath = "/"
    , vrMethod = "GET"
    , vrTargetHostSource = TargetNodeConnectionHost
    , vrFollowRedirects = False
    , vrMaxRedirects = 5
    , vrTimeoutSeconds = 10
    , vrTlsMode = TlsStrict
    }

mkSpec :: [VerificationConstraint] -> EnvVerificationSpec
mkSpec constraints =
  EnvVerificationSpec
    { evEnable = True
    , evEnforce = True
    , evChecks =
        [ VerificationCheck
            { vcName = "drupal-homepage"
            , vcType = "httpHostHeaderCurl"
            , vcRequest = mkRequest
            , vcConstraints = constraints
            }
        ]
    }

mkCurlOutput :: Text -> Int -> Text
mkCurlOutput body status =
  body
    <> "\n__HOSTENV_PROVIDER_HTTP_STATUS__:"
    <> fromInt status
    <> "\n__HOSTENV_PROVIDER_EFFECTIVE_URL__:http://www.example.test/\n"
  where
    fromInt = T.pack . show

main :: IO ()
main = do
  assert
    (parseEnvVerificationSpec KM.empty == Right defaultEnvVerificationSpec)
    "missing deploymentVerification should use defaults"

  resultsPass <-
    runEnvVerificationWith
      (\_ -> pure (Exit.ExitSuccess, showText (mkCurlOutput "<meta name=\"Generator\" content=\"Drupal 11\" />" 200), ""))
      "backend04.hostenv.sh"
      (mkSpec [AllowNonZeroExitStatus False, MinHttpStatus 200, MaxHttpStatus 299, StdoutRegexMustMatch "Generator"])
  let passResult = case firstResult resultsPass of
        VerificationCheckResult {vcrPassed = passed} -> passed
  assert passResult "200 response with generator should pass"

  resultsFail <-
    runEnvVerificationWith
      (\_ -> pure (Exit.ExitSuccess, showText (mkCurlOutput "Moved Permanently" 301), ""))
      "backend04.hostenv.sh"
      (mkSpec [AllowNonZeroExitStatus False, MinHttpStatus 200, MaxHttpStatus 399, StdoutRegexMustMatch "Generator"])
  let failResult = case firstResult resultsFail of
        VerificationCheckResult {vcrPassed = passed} -> passed
  assert (not failResult) "301 response without generator should fail by default"

  resultsRedirectOk <-
    runEnvVerificationWith
      (\_ -> pure (Exit.ExitSuccess, showText (mkCurlOutput "Moved Permanently" 301), ""))
      "backend04.hostenv.sh"
      ( mkSpec
          [ AllowNonZeroExitStatus False
          , MinHttpStatus 200
          , MaxHttpStatus 399
          , SkipStdoutRegexOnRedirect True
          , StdoutRegexMustMatch "Generator"
          ]
      )
  let redirectOkResult = firstResult resultsRedirectOk
  let redirectOkPassed = case redirectOkResult of
        VerificationCheckResult {vcrPassed = passed} -> passed
  assert redirectOkPassed "301 response should pass when skipStdoutRegexOnRedirect=true"

  putStrLn "ok"

showText :: Text -> String
showText = T.unpack

firstResult :: [a] -> a
firstResult results =
  case results of
    (x : _) -> x
    [] -> error "expected at least one verification result"
