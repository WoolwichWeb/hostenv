{-# LANGUAGE ImportQualifiedPost #-}

module Hostenv.Provider.DnsPointsToCache
  ( DnsPointsToCache
  , cacheDnsPointsTo
  , newDnsPointsToCache
  ) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T

-- | An in-memory cache for answers to checks that determine whether one DNS
-- name points to another.
newtype DnsPointsToCache = DnsPointsToCache (IORef (M.Map (Text, Text) Bool))

-- | Create an empty cache for checks that determine whether one DNS name
-- points to another. Cached answers remain available until this value is
-- discarded.
newDnsPointsToCache :: IO DnsPointsToCache
newDnsPointsToCache = DnsPointsToCache <$> newIORef M.empty

normalizeDnsName :: Text -> Text
normalizeDnsName = T.toLower . T.dropWhileEnd (== '.') . T.strip

-- | Check whether one DNS name points to another, reusing an answer already
-- stored in the cache. The supplied check runs only when the cache has not seen
-- the same pair of names.
cacheDnsPointsTo :: DnsPointsToCache -> (Text -> Text -> IO Bool) -> Text -> Text -> IO Bool
cacheDnsPointsTo (DnsPointsToCache cacheRef) pointsTo vhost expectedHost = do
  let key = (normalizeDnsName vhost, normalizeDnsName expectedHost)
  cache <- readIORef cacheRef
  case M.lookup key cache of
    Just result -> pure result
    Nothing -> do
      result <- pointsTo vhost expectedHost
      modifyIORef' cacheRef (M.insert key result)
      pure result
