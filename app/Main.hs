module Main where

import Relude
import Oasis.Config
import Oasis.Types
import Oasis.Runner.Chat
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  let (alias, prompt) = case args of
        (a:rest) -> (toText a, if null rest then Nothing else Just (T.unwords (map toText rest)))
        []       -> ("deepseek", Nothing)

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
              case prompt of
                Nothing -> pure ()
                Just q -> do
                  putTextLn "--- Running single-turn chat ---"
                  result <- runSingleTurn p key q
                  case result of
                    Left err -> do
                      putTextLn $ "Request failed: " <> err
                      exitFailure
                    Right resp -> BL8.putStrLn (encode resp)
