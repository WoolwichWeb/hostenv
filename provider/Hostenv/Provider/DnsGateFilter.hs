{-# LANGUAGE ImportQualifiedPost #-}

module Hostenv.Provider.DnsGateFilter
    ( filterEnvironmentsByNode
    ) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)

nodeKey :: K.Key
nodeKey = K.fromString "node"

filterEnvironmentsByNode :: Maybe Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
filterEnvironmentsByNode Nothing envs = envs
filterEnvironmentsByNode (Just selectedNode) envs =
    KM.filter (matchesSelectedNode selectedNode) envs
  where
    matchesSelectedNode nodeName (A.Object envObj) =
        case KM.lookup nodeKey envObj of
            Just (A.String envNode) -> envNode == nodeName
            _ -> False
    matchesSelectedNode _ _ = False
