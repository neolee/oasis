module Oasis.Config
  ( findConfig
  , loadConfig
  , resolveProvider
  ) where

import Relude
import Oasis.Types
import qualified Toml
import Toml (decode)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import System.Directory (doesFileExist)

-- | Look for config file in standard locations
findConfig :: IO (Maybe FilePath)
findConfig = do
  let paths = ["providers.toml", "config/providers.toml", "local/providers.toml"]
  findFirstM doesFileExist paths

findFirstM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findFirstM _ [] = pure Nothing
findFirstM predicate (x:xs) = do
  ok <- predicate x
  if ok then pure (Just x) else findFirstM predicate xs

loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
  content <- TIO.readFile path
  case decode @Config content of
    Toml.Failure errs -> pure $ Left (T.intercalate "\n" (map (T.pack . show) errs))
    Toml.Success _ cfg -> pure $ Right cfg

resolveProvider :: Config -> Text -> IO (Maybe (Provider, Text))
resolveProvider cfg alias = do
  let providerName = fromMaybe alias (M.lookup alias (aliases cfg))
  case M.lookup providerName (providers cfg) of
    Nothing -> pure Nothing
    Just baseProvider -> do
      apiKey <- fromMaybe "" <$> lookupEnv (toString (api_key_name baseProvider))
      let defaultType = Just (model_type (defaults cfg))
          providerWithDefaults = baseProvider { default_model_type = default_model_type baseProvider <|> defaultType }
      pure $ Just (providerWithDefaults, toText apiKey)
