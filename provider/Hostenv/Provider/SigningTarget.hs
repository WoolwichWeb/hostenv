{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.SigningTarget
    ( deployProfilePathInstallable
    )
where

import Data.Text (Text)
import Data.Text qualified as T

deployProfilePathInstallable :: Text -> Text -> Text
deployProfilePathInstallable nodeName profileName =
    "generated/.#deploy.nodes."
        <> nixAttrToken nodeName
        <> ".profiles."
        <> nixAttrToken profileName
        <> ".path"

nixAttrToken :: Text -> Text
nixAttrToken token =
    "\"" <> escapeAttrToken token <> "\""

escapeAttrToken :: Text -> Text
escapeAttrToken =
    T.concatMap (\c ->
        case c of
            '"' -> "\\\""
            '\\' -> "\\\\"
            _ -> T.singleton c
    )
