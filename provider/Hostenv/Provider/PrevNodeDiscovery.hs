{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.PrevNodeDiscovery
    ( PrevNodeResolution (..)
    , canonicalHostInDomain
    , discoverMatchingNodes
    , resolvePrevNodeFromMatches
    ) where

import Control.Monad (filterM)
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T

stripDot :: Text -> Text
stripDot = T.dropWhileEnd (== '.')

normalizeHost :: Text -> Text
normalizeHost = T.toLower . stripDot . T.strip

canonicalHostInDomain :: Text -> Text -> Text
canonicalHostInDomain label hostenvHostname =
    let domain = normalizeHost hostenvHostname
        labelNorm = normalizeHost label
        suffix = "." <> domain
     in
        if T.null labelNorm
            then labelNorm
            else
                if T.isSuffixOf suffix labelNorm
                    then labelNorm
                    else labelNorm <> suffix

discoverMatchingNodes :: (Text -> Text -> IO Bool) -> Text -> Text -> [Text] -> IO [Text]
discoverMatchingNodes pointsTo hostenvHostname envName candidateNodes = do
    let envHost = canonicalHostInDomain envName hostenvHostname
    let normalizedNodes = sort (nub (filter (not . T.null) (map normalizeHost candidateNodes)))
    filterM (\node -> pointsTo envHost (canonicalHostInDomain node hostenvHostname)) normalizedNodes

data PrevNodeResolution
    = PrevNodeResolved Text
    | PrevNodeSkip
    | PrevNodeAmbiguousFatal [Text]
    deriving (Eq, Show)

resolvePrevNodeFromMatches :: Text -> [Text] -> PrevNodeResolution
resolvePrevNodeFromMatches currentNode matches =
    let currentNodeNorm = normalizeHost currentNode
        uniqueMatches = sort (nub (filter (not . T.null) (map normalizeHost matches)))
     in case uniqueMatches of
            [] -> PrevNodeSkip
            [node] ->
                if node == currentNodeNorm
                    then PrevNodeSkip
                    else PrevNodeResolved node
            many ->
                if currentNodeNorm `elem` many
                    then PrevNodeSkip
                    else PrevNodeAmbiguousFatal many
