module Main where

import Data.Aeson
import qualified Data.Csv as CSV
import Lib

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  contents <- BSL.readFile "issues.json"
  case (eitherDecode contents :: Either String (Data Task)) of
    Right dt -> do
      issues <- doImport $ _dataData dt
      putStrLn $ show issues
    Left e -> putStrLn $ show e
