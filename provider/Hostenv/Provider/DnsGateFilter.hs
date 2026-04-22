{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hostenv.Provider.DnsGateFilter
    ( DnsGateItem (..)
    , collectDnsGateItems
    , filterEnvironmentsByNode
    ) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Hostenv.Provider.PrevNodeDiscovery qualified as PrevNode

data DnsGateItem = DnsGateItem
    { dgiEnvName :: Text
    , dgiNodeName :: Maybe Text
    , dgiVhostName :: Text
    , dgiDiscoveryHost :: Text
    , dgiExpectedHost :: Text
    }

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

collectDnsGateItems :: Text -> KM.KeyMap A.Value -> [DnsGateItem]
collectDnsGateItems hostenvHostname envs =
    concatMap collectEnv (KM.toList envs)
  where
    collectEnv (kEnv, vEnv) =
        case vEnv of
            A.Object envObj ->
                let envName = K.toText kEnv
                    nodeName = lookupText nodeKey envObj
                    expectedHostFor vh = maybe vh (`PrevNode.canonicalHostInDomain` hostenvHostname) nodeName
                    vhosts = fromMaybe KM.empty (lookupObj (K.fromString "virtualHosts") envObj)
                 in map
                        (\(vhKey, _) ->
                            let vhName = K.toText vhKey
                             in DnsGateItem
                                    { dgiEnvName = envName
                                    , dgiNodeName = nodeName
                                    , dgiVhostName = vhName
                                    , dgiDiscoveryHost = vhName
                                    , dgiExpectedHost = expectedHostFor vhName
                                    }
                        )
                        (KM.toList vhosts)
            _ -> []

lookupText :: K.Key -> KM.KeyMap A.Value -> Maybe Text
lookupText key obj =
    case KM.lookup key obj of
        Just (A.String t) -> Just t
        _ -> Nothing

lookupObj :: K.Key -> KM.KeyMap A.Value -> Maybe (KM.KeyMap A.Value)
lookupObj key obj =
    case KM.lookup key obj of
        Just (A.Object o) -> Just o
        _ -> Nothing
