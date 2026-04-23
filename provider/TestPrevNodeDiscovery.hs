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
    ( classifyProbe (NodeName "node-b") (Probe (Hostname "env-main-a1b2c3.hosting.test") [])
        == ProbeSkipped ProbeNoMatches
    )
    "empty matches should skip discovery"
  assert
    ( classifyProbe (NodeName "node-b") (Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a"])
        == ProbeResolved (NodeName "node-a")
    )
    "single non-current match should resolve previous node"
  assert
    ( classifyProbe (NodeName "node-b") (Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-b"])
        == ProbeSkipped (ProbeMatchedCurrent [NodeName "node-b"])
    )
    "single current-node match should skip discovery"
  assert
    ( classifyProbe (NodeName "node-b") (Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a", NodeName "node-b"])
        == ProbeSkipped (ProbeMatchedCurrent [NodeName "node-a", NodeName "node-b"])
    )
    "ambiguous matches including current node should skip discovery"
  assert
    ( classifyProbe (NodeName "node-c") (Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a", NodeName "node-b"])
        == ProbeAmbiguous [NodeName "node-a", NodeName "node-b"]
    )
    "ambiguous matches excluding current node should be fatal"
  assert
    ( probeHosts "hosting.test" "env-main-a1b2c3" ["www.customer.com", "ENV-MAIN-A1B2C3.HOSTING.TEST."]
        == [Hostname "env-main-a1b2c3.hosting.test", Hostname "www.customer.com"]
    )
    "probeHosts should probe canonical host first and deduplicate normalized vhosts"
  assert
    ( chooseDiscoveryOutcome
        (NodeName "node-b")
        [ Probe (Hostname "env-main-a1b2c3.hosting.test") []
        ]
        == DiscoverySkipped DiscoveryNoMatches
    )
    "host resolution should preserve the no-match skip reason"
  assert
    ( chooseDiscoveryOutcome
        (NodeName "node-b")
        [ Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a", NodeName "node-b"]
        ]
        == DiscoverySkipped (DiscoveryMatchedCurrent (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a", NodeName "node-b"])
    )
    "host resolution should preserve when a probed hostname matched the current node"
  assert
    ( chooseDiscoveryOutcome
        (NodeName "node-b")
        [ Probe (Hostname "env-main-a1b2c3.hosting.test") []
        , Probe (Hostname "www.customer.com") [NodeName "node-a"]
        ]
        == DiscoveryResolved (Hostname "www.customer.com") (NodeName "node-a")
    )
    "vhost fallback should resolve previous node when canonical host has no match"
  assert
    ( chooseDiscoveryOutcome
        (NodeName "node-c")
        [ Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a", NodeName "node-b"]
        , Probe (Hostname "www.customer.com") [NodeName "node-a"]
        ]
        == DiscoveryResolved (Hostname "www.customer.com") (NodeName "node-a")
    )
    "vhost fallback should win when canonical host is ambiguous"

  putStrLn "ok"
