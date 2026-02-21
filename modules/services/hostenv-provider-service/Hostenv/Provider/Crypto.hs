{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Hostenv.Provider.Crypto
  ( TokenCipher(..)
  , EncryptedToken(..)
  , loadTokenCipher
  , encryptToken
  , decryptToken
  ) where

import "crypton" Crypto.Cipher.AES (AES256)
import "crypton" Crypto.Cipher.Types
  ( AEADMode (..)
  , AuthTag (..)
  , aeadInit
  , aeadSimpleDecrypt
  , aeadSimpleEncrypt
  , cipherInit
  )
import "crypton" Crypto.Error (CryptoFailable (..))
import "crypton" Crypto.Hash (Digest, SHA256, hash)
import "crypton" Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data TokenCipher = TokenCipher
  { tokenCipherKeyId :: Text
  , tokenCipherKey :: BS.ByteString
  } deriving (Eq, Show)

data EncryptedToken = EncryptedToken
  { encryptedTokenCiphertext :: BS.ByteString
  , encryptedTokenNonce :: BS.ByteString
  , encryptedTokenKeyId :: Text
  } deriving (Eq, Show)

loadTokenCipher :: FilePath -> IO (Either Text TokenCipher)
loadTokenCipher path = do
  raw <- T.pack <$> readFile path
  let linesRaw = map T.strip (T.lines raw)
      linesFiltered = filter (\line -> line /= "" && not (T.isPrefixOf "#" line)) linesRaw
      kvs = mapMaybe parseKv linesFiltered
      bareValues = filter (not . T.isInfixOf "=") linesFiltered
      mKeyRaw =
        lookupFirst ["key", "key_hex", "GITLAB_TOKEN_ENCRYPTION_KEY"] kvs
          <> listToMaybe bareValues
      mKeyIdRaw = lookupFirst ["key_id", "keyId", "GITLAB_TOKEN_ENCRYPTION_KEY_ID"] kvs
  case mKeyRaw of
    Nothing -> pure (Left "token encryption key file missing `key=` entry")
    Just keyRaw ->
      case decodeKeyMaterial keyRaw of
        Left err -> pure (Left err)
        Right keyBytes ->
          if BS.length keyBytes /= 32
            then pure (Left "token encryption key must be exactly 32 bytes")
            else do
              let keyIdDefault = defaultKeyId keyBytes
                  keyId = fromMaybeText keyIdDefault mKeyIdRaw
              pure (Right (TokenCipher keyId keyBytes))

encryptToken :: TokenCipher -> Text -> IO (Either Text EncryptedToken)
encryptToken cipher token = do
  nonce <- getRandomBytes 12 :: IO BS.ByteString
  pure (encryptTokenWithNonce cipher nonce token)

decryptToken :: TokenCipher -> BS.ByteString -> BS.ByteString -> Text -> Either Text Text
decryptToken cipher nonce ciphertext keyId
  | keyId /= cipher.tokenCipherKeyId = Left "token encryption key id mismatch"
  | BS.length nonce /= 12 = Left "encrypted token nonce has invalid length"
  | BS.length ciphertext <= 16 = Left "encrypted token payload is invalid"
  | otherwise =
      case cipherInit cipher.tokenCipherKey :: CryptoFailable AES256 of
        CryptoFailed err -> Left ("token cipher init failed: " <> T.pack (show err))
        CryptoPassed aes ->
          case aeadInit AEAD_GCM aes nonce of
            CryptoFailed err -> Left ("token AEAD init failed: " <> T.pack (show err))
            CryptoPassed aead ->
              let (cipherOnly, tagBytesRaw) = BS.splitAt (BS.length ciphertext - 16) ciphertext
                  authTag = AuthTag (BA.convert tagBytesRaw)
               in case aeadSimpleDecrypt aead BS.empty cipherOnly authTag of
                    Nothing -> Left "failed to decrypt token payload"
                    Just plaintext ->
                      case TE.decodeUtf8' plaintext of
                        Left _ -> Left "decrypted token payload is not valid UTF-8"
                        Right decoded -> Right decoded

encryptTokenWithNonce :: TokenCipher -> BS.ByteString -> Text -> Either Text EncryptedToken
encryptTokenWithNonce cipher nonce token =
  case cipherInit cipher.tokenCipherKey :: CryptoFailable AES256 of
    CryptoFailed err -> Left ("token cipher init failed: " <> T.pack (show err))
    CryptoPassed aes ->
      case aeadInit AEAD_GCM aes nonce of
        CryptoFailed err -> Left ("token AEAD init failed: " <> T.pack (show err))
        CryptoPassed aead ->
          let (authTag, ciphertext) = aeadSimpleEncrypt aead BS.empty (TE.encodeUtf8 token) 16
              tagBytes = BA.convert authTag :: BS.ByteString
           in Right
                EncryptedToken
                  { encryptedTokenCiphertext = ciphertext <> tagBytes
                  , encryptedTokenNonce = nonce
                  , encryptedTokenKeyId = cipher.tokenCipherKeyId
                  }

parseKv :: Text -> Maybe (Text, Text)
parseKv line =
  case T.breakOn "=" line of
    (k, v) | v /= "" -> Just (T.strip k, T.strip (T.drop 1 v))
    _ -> Nothing

lookupFirst :: [Text] -> [(Text, Text)] -> Maybe Text
lookupFirst keys kvs =
  listToMaybe [v | (k, v) <- kvs, k `elem` keys]

decodeKeyMaterial :: Text -> Either Text BS.ByteString
decodeKeyMaterial keyRaw =
  let compact = T.filter (\c -> c /= ' ' && c /= '\t' && c /= '\r' && c /= '\n') keyRaw
   in if T.length compact == 64 && T.all isHexDigit compact
        then case BAE.convertFromBase BAE.Base16 (TE.encodeUtf8 compact) of
          Left _ -> Left "failed to decode hex token encryption key"
          Right bs -> Right bs
        else
          let rawBytes = TE.encodeUtf8 compact
           in if BS.length rawBytes == 32
                then Right rawBytes
                else Left "token encryption key must be 64 hex chars or 32 raw bytes"

defaultKeyId :: BS.ByteString -> Text
defaultKeyId keyBytes =
  let digest = hash keyBytes :: Digest SHA256
      digestHex = TE.decodeUtf8 (BAE.convertToBase BAE.Base16 digest)
   in T.take 16 digestHex

fromMaybeText :: Text -> Maybe Text -> Text
fromMaybeText fallback mValue =
  case mValue of
    Just value | T.strip value /= "" -> T.strip value
    _ -> fallback
