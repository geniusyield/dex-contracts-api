{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Crypto (
  -- * Re-Exports
  ToData,

  -- * Signed message
  SignedMessage,

  -- * Message signing
  signMessage,

  -- * Signature verification
  verifySignedMessageOffChain,

  -- * Offchain types for mimicking onchain signature flow
  SignatureOffchain (..),
  SignedMessageOffchain (..),
  signedMessageToOnchain,
) where

import Cardano.Api qualified as Api
import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Crypto
import Data.Aeson qualified as Aeson
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding qualified as TE
import GeniusYield.Imports
import GeniusYield.OnChain.Core.Common.Crypto (SignedMessage (..))
import GeniusYield.Types
import PlutusLedgerApi.V1.Scripts (DatumHash (..))
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.IsData (ToData (toBuiltinData))
import PlutusTx.Prelude (verifyEd25519Signature)

newtype SignatureOffchain = SignatureOffchain {getSignatureOffchain :: ByteString}
  deriving (Eq, Generic, Show)

instance ToJSON SignatureOffchain where
  toEncoding (SignatureOffchain x) = toEncoding . TE.decodeUtf8 $ Base16.encode x
  toJSON (SignatureOffchain x) = toJSON . TE.decodeUtf8 $ Base16.encode x

instance FromJSON SignatureOffchain where
  parseJSON =
    Aeson.withText "SignatureOffchain" $
      either fail (pure . SignatureOffchain) . Base16.decode . TE.encodeUtf8

data SignedMessageOffchain a = SignedMessageOffchain
  { -- | The cryptographic signature.
    smoSignature :: SignatureOffchain,
    -- | The hash of the message.
    smoMessageHash :: GYDatumHash
  }
  deriving (FromJSON, Generic, Show, ToJSON)

signedMessageToOnchain :: SignedMessageOffchain a -> SignedMessage a
signedMessageToOnchain = signedMessageToOnchainUnbound

signedMessageToOnchainUnbound :: SignedMessageOffchain a -> SignedMessage b
signedMessageToOnchainUnbound SignedMessageOffchain {..} =
  SignedMessage
    (PlutusTx.toBuiltin $ getSignatureOffchain smoSignature)
    (datumHashToPlutus smoMessageHash)

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import GeniusYield.Types
-}

{- | Signs a message with a specified signing key.

>>> signMessage True "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
SignedMessageOffchain {smoSignature = SignatureOffchain {getSignatureOffchain = "\241\v@\148\162\245\215\224\163\159\241\&9\rq\US<'\141\"\184Z\247\133b\\e\173P4]\150\t\151\DLEl\194UdWy5\244\205s\NAK\207\164}?\209\218\SYN\221m\174\252\247f'\153C\DC4\232\EOT"}, smoMessageHash = GYDatumHash "8392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e"}
-}
signMessage
  :: forall a
   . (Show a, ToData a)
  => a
  -- ^ The message to sign.
  -> GYPaymentSigningKey
  -- ^ The key to sign the message with.
  -> SignedMessageOffchain a
signMessage msg skey = case Crypto.secretKey $ Api.serialiseToRawBytes $ paymentSigningKeyToApi skey of
  CryptoFailed _ -> error "impossible case: secret keys should always be valid"
  CryptoPassed skey' ->
    let
      pkey = Crypto.toPublic skey'
      d = datumFromPlutus' $ toBuiltinData msg
      dh = hashDatum d
      msg' = Api.serialiseToRawBytes $ datumHashToApi dh
      sig = Crypto.sign skey' pkey msg'
     in
      SignedMessageOffchain
        { smoSignature = SignatureOffchain $ BA.convert sig,
          smoMessageHash = dh
        }

{- | Verifies a signed message.

>>> let skey = "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
>>> let vkey = paymentVerificationKey skey
>>> let signed = signedMessageToOnchain $ signMessage True skey
>>> verifySignedMessageOffChain vkey signed True
True

>>> let skey = "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
>>> let vkey = paymentVerificationKey skey
>>> let signed = signedMessageToOnchain $ signMessage True skey
>>> verifySignedMessageOffChain vkey signed False
False

>>> let skey = "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
>>> let vkey = "0903b319b0d21c823b725bcdd098acb74d4313b60ec36a3f484053791e596ef1"
>>> let signed = signedMessageToOnchain $ signMessage True skey
>>> verifySignedMessageOffChain vkey signed True
False
-}
verifySignedMessageOffChain
  :: (Show a, ToData a)
  => GYPaymentVerificationKey
  -- ^ The verification key of the signer.
  -> SignedMessage a
  -- ^ The signed message.
  -> a
  -- ^ The message.
  -> Bool
verifySignedMessageOffChain pk SignedMessage {smSignature, smMessageHash} a
  | not $ verifyEd25519Signature pk' h smSignature = False
  | otherwise = smMessageHash == coerce (datumHashToPlutus $ hashDatum $ datumFromPlutus' $ toBuiltinData a)
 where
  pk' = coerce . PlutusTx.toBuiltin $ paymentVerificationKeyRawBytes pk
  h = coerce smMessageHash
