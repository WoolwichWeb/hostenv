{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hostenv.Provider.DnsPointsToCache
import System.Exit (exitFailure)

assert :: Bool -> String -> IO ()
assert cond msg = unless cond $ do
    putStrLn ("FAIL: " <> msg)
    exitFailure

main :: IO ()
main = do
    cache <- newDnsPointsToCache
    callsRef <- newIORef ([] :: [(String, String)])
    let pointsTo vhost expectedHost = do
            modifyIORef' callsRef (<> [(show vhost, show expectedHost)])
            pure (vhost == "WWW.EXAMPLE.TEST." && expectedHost == "node-a.hosting.test")

    first <- cacheDnsPointsTo cache pointsTo "WWW.EXAMPLE.TEST." "node-a.hosting.test"
    second <- cacheDnsPointsTo cache pointsTo "www.example.test" "NODE-A.HOSTING.TEST."
    third <- cacheDnsPointsTo cache pointsTo "www.example.test" "node-b.hosting.test"

    assert first "the first request should run the supplied DNS check"
    assert second "equivalent DNS names should reuse the cached answer"
    assert (not third) "a different expected host should run the supplied DNS check"
    calls <- readIORef callsRef
    assert (length calls == 2) "equivalent DNS names should share a cache entry"
    putStrLn "ok"
