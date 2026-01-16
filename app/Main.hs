module Main where

import Relude
import Oasis.Config
import Oasis.Types

main :: IO ()
main = do
  args <- getArgs
  let alias = case args of
                (a:_) -> toText a
                []    -> "deepseek"

  putTextLn $ "Loading config for alias: " <> alias
  
  mPath <- findConfig
  case mPath of
    Nothing -> do
      putTextLn "Error: providers.toml not found in current directory, config/, or local/."
      exitFailure
    Just path -> do
      cfgResult <- loadConfig path
      case cfgResult of
        Left err -> do
          putTextLn $ "Error parsing config: " <> err
          exitFailure
        Right cfg -> do
          res <- resolveProvider cfg alias
          case res of
            Nothing -> putTextLn $ "Provider not found for alias: " <> alias
            Just (p, key) -> do
              putTextLn "--- Resolved Provider ---"
              print p
              if key /= ""
                then putTextLn "API Key: Found (hidden)"
                else putTextLn "API Key: NOT FOUND (Check environment variables)"
