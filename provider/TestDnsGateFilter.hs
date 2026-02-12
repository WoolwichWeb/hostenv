{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hostenv.Provider.DnsGateFilter (filterEnvironmentsByNode)
import System.Exit (exitFailure)

mkEnv :: Maybe Text -> A.Value
mkEnv mNode =
    A.Object $
        KM.fromList
            [ (K.fromString "node", maybe A.Null A.String mNode)
            ]

keysSet :: KM.KeyMap A.Value -> S.Set Text
keysSet =
    S.fromList . map (K.toText . fst) . KM.toList

assertSetEqual :: Text -> S.Set Text -> S.Set Text -> IO ()
assertSetEqual label expected actual =
    if expected == actual
        then pure ()
        else do
            TIO.putStrLn ("assertion failed (" <> label <> ")")
            TIO.putStrLn ("expected: " <> tShow expected)
            TIO.putStrLn ("actual:   " <> tShow actual)
            exitFailure

tShow :: Show a => a -> Text
tShow = T.pack . show

main :: IO ()
main = do
    let envs =
            KM.fromList
                [ (K.fromString "envA", mkEnv (Just "backend01"))
                , (K.fromString "envB", mkEnv (Just "backend05"))
                , (K.fromString "envC", mkEnv Nothing)
                , (K.fromString "broken", A.String "not-an-object")
                ]

    assertSetEqual
        "no node filter keeps all environments"
        (S.fromList ["envA", "envB", "envC", "broken"])
        (keysSet (filterEnvironmentsByNode Nothing envs))

    assertSetEqual
        "node filter keeps only matching node"
        (S.fromList ["envB"])
        (keysSet (filterEnvironmentsByNode (Just "backend05") envs))

    assertSetEqual
        "node filter keeps nothing when no environment matches"
        S.empty
        (keysSet (filterEnvironmentsByNode (Just "backend99") envs))

    TIO.putStrLn "ok"
