module Main where

import Relude
import Graphics.Vty.UnicodeWidthTable.Main (defaultMain)
import System.Console.ANSI (getCursorPosition)

main :: IO ()
main = defaultMain charWidth

attempts :: Int
attempts = 3

charWidth :: Char -> IO Int
charWidth = charWidth' 0

charWidth' :: Int -> Char -> IO Int
charWidth' n c
  | n >= attempts = do
      putStrLn ("\rUnable to check: " <> [c] <> " (" <> show c <> ")")
      pure 1
  | otherwise = do
      putStr ['\r', c]
      mb <- getCursorPosition
      case mb of
        Nothing -> charWidth' (n + 1) c
        Just (_, col) -> pure col
