{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
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
    let planWithStateOnlyNode =
            asObject $
                A.object
                    [ "nodes"
                        .= A.object
                            [ "node-b" .= A.object []
                            ]
                    , "nodeConnections"
                        .= A.object
                            [ "node-a" .= A.object []
                            , "node-b" .= A.object []
                            ]
                    ]
    assert
        (discoveryNodeNames planWithStateOnlyNode ["node-b", "node-c"] == ["node-a", "node-b", "node-c"])
        "discovery nodes should include state-only nodeConnections and current env nodes"
    assert
        ( previousNodeCandidates
            (NodeName "node-b")
            [NodeName "node-b", NodeName "NODE-B.", NodeName "node-c"]
            == [NodeName "node-c"]
        )
        "previousNodeCandidates should normalize names and exclude the current node"
    assert
        ( resolveDeclaredPrevNode "node-b" Nothing (Just "node-a")
            == DeclaredPreviousNode PlanPreviousNode "node-a"
        )
        "plan previousNode should resolve before DNS discovery"
    assert
        ( resolveDeclaredPrevNode "node-b" (Just "node-c") (Just "node-a")
            == DeclaredPreviousNode MigrationSourceOverride "node-c"
        )
        "--migration-source should take precedence over plan previousNode"
    assert
        ( resolveDeclaredPrevNode "node-b" (Just "NODE-B.") (Just "node-a")
            == DeclaredCurrentNode MigrationSourceOverride "NODE-B."
        )
        "a declared current node should mean that no migration is needed"

    assert
        ( classifyProbe (NodeName "node-b") (Probe (Hostname "env-main-a1b2c3.hosting.test") [])
            == ProbeSkipped ProbeNoMatches
        )
        "empty matches should skip discovery"
    assert
        ( classifyProbe (NodeName "node-b") (Probe (Hostname "env-main-a1b2c3.hosting.test") [NodeName "node-a"])
            == ProbeResolved (NodeName "node-a")
        )
        "a single possible previous node should be selected"
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
        "multiple possible previous nodes should fail discovery"
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

asObject :: A.Value -> KM.KeyMap A.Value
asObject value =
    case value of
        A.Object obj -> obj
        _ -> error "expected JSON object"
