{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Hostenv.Provider.PrevNodeDiscovery
import System.Exit (exitFailure)

assert :: Bool -> String -> IO ()
assert cond msg = unless cond $ do
  putStrLn ("FAIL: " <> msg)
  exitFailure

main :: IO ()
main = do
  assert
    (canonicalHostInDomain "env-main-a1b2c3" "hosting.test" == "env-main-a1b2c3.hosting.test")
    "canonical host should append hostenv domain"
  assert
    (canonicalHostInDomain "env-main-a1b2c3.hosting.test" "hosting.test" == "env-main-a1b2c3.hosting.test")
    "canonical host should not duplicate suffix"

  assert
    (resolvePrevNodeFromMatches "node-b" [] == PrevNodeSkip)
    "empty matches should skip discovery"
  assert
    (resolvePrevNodeFromMatches "node-b" ["node-a"] == PrevNodeResolved "node-a")
    "single non-current match should resolve previous node"
  assert
    (resolvePrevNodeFromMatches "node-b" ["node-b"] == PrevNodeSkip)
    "single current-node match should skip discovery"
  assert
    (resolvePrevNodeFromMatches "node-b" ["node-a", "node-b"] == PrevNodeSkip)
    "ambiguous matches including current node should skip discovery"
  assert
    ( resolvePrevNodeFromMatches "node-c" ["node-a", "node-b"]
        == PrevNodeAmbiguousFatal ["node-a", "node-b"]
    )
    "ambiguous matches excluding current node should be fatal"

  putStrLn "ok"
