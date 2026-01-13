{-# LANGUAGE OverloadedStrings #-}

module GeniusYield.Api.Oracle.Http (
  OraclePriceFraction (..),
  OracleCertificateApiResponse (..),
  fetchOracleQuote,
  assetToOracleIdentifiers,
) where

import Control.Exception (displayException)
import Data.Aeson qualified as Aeson
import Data.Ratio ((%))
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import GeniusYield.Types
import Network.HTTP.Simple

-- | Fraction returned by the oracle price endpoint.
data OraclePriceFraction = OraclePriceFraction
  { numerator :: !Integer,
    denominator :: !Integer
  }
  deriving stock (Show)

instance Aeson.FromJSON OraclePriceFraction where
  parseJSON = Aeson.withObject "OraclePriceFraction" $ \v ->
    OraclePriceFraction
      <$> v Aeson..: "numerator"
      <*> v Aeson..: "denominator"

-- | Response body expected from the oracle price endpoint.
data OracleCertificateApiResponse = OracleCertificateApiResponse
  { oarStatus :: !Text.Text,
    oarPrice :: !OraclePriceFraction,
    oarTimestamp :: !UTCTime
  }
  deriving stock (Show)

instance Aeson.FromJSON OracleCertificateApiResponse where
  parseJSON = Aeson.withObject "OracleCertificateApiResponse" $ \v ->
    OracleCertificateApiResponse
      <$> v Aeson..: "status"
      <*> v Aeson..: "price"
      <*> v Aeson..: "timestamp"

-- | Convert a 'GYAssetClass' into oracle endpoint identifiers.
assetToOracleIdentifiers :: GYAssetClass -> (Text.Text, Text.Text)
assetToOracleIdentifiers GYLovelace = ("", "")
assetToOracleIdentifiers (GYToken policyId tokenName) =
  ( mintingPolicyIdToText policyId,
    tokenNameToHex tokenName
  )

{- | Fetch a price quote from the oracle HTTP endpoint.
Returns a rational price and timestamp on success, or an error message.
-}
fetchOracleQuote
  :: String
  -> GYAssetClass
  -> GYAssetClass
  -> IO (Either String (Rational, UTCTime))
fetchOracleQuote _ baseAsset quoteAsset | baseAsset == quoteAsset = do
  now <- getCurrentTime
  pure $ Right (1 % 1, now)
fetchOracleQuote endpoint baseAsset quoteAsset = do
  req0 <- parseRequest endpoint
  let
    (baseSymbol, baseToken) = assetToOracleIdentifiers baseAsset
    (quoteSymbol, quoteToken) = assetToOracleIdentifiers quoteAsset
    body =
      Aeson.object
        [ "baseSymbol" Aeson..= baseSymbol,
          "baseToken" Aeson..= baseToken,
          "quoteSymbol" Aeson..= quoteSymbol,
          "quoteToken" Aeson..= quoteToken
        ]
    req =
      setRequestMethod "POST" $
        setRequestBodyJSON body req0
  resp <- httpJSONEither req
  case getResponseBody resp of
    Left err ->
      pure . Left $ "oracle endpoint decode failure: " <> displayException err
    Right OracleCertificateApiResponse {..} -> do
      let OraclePriceFraction {..} = oarPrice
      pure $ do
        _ <-
          if denominator == 0
            then Left "oracle endpoint returned zero denominator"
            else Right ()
        _ <- case oarStatus of
          "average" -> Right ()
          "source-fail" -> Right ()
          other -> Left $ "oracle endpoint returned unexpected status " <> Text.unpack other
        let price = numerator % denominator
        pure (price, oarTimestamp)
