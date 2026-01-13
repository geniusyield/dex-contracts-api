module GeniusYield.Config.Utils (
  fillPlaceholders,
  coreConfigIO',
) where

import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text (Text, pack, replace)
import Data.Text.IO qualified as TextIO (readFile)
import GeniusYield.GYConfig
import System.Environment (getEnvironment)

fillPlaceholders :: Text.Text -> IO Text.Text
fillPlaceholders str = do
  env <- getEnvironment
  return $ foldl step str env
 where
  step s (k, v) = Text.replace ("<<" <> k' <> ">>") v' s
   where
    k' = Text.pack k
    v' = Text.pack v

-- | Read the CoreConfig file and replace secret placeholders with environment variables (TODO: push into `atlas` after resolving version conflicts)
coreConfigIO' :: FilePath -> IO GYCoreConfig
coreConfigIO' file = do
  t <- TextIO.readFile file >>= fillPlaceholders
  case Aeson.eitherDecodeStrictText t of
    Left err -> throwIO $ userError err
    Right cfg -> pure cfg
