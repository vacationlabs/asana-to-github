module Main where

import Data.Aeson
import qualified Data.Csv as CSV
import Lib
import Control.Lens

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  contents <- BSL.readFile "issues.json"
  case (eitherDecode contents :: Either String (Data Task)) of
    Right dt -> do
      --issues <- doImport $ filter (\a -> a ^. Lib.id ==  47822546634645) (_dataData dt)  
      issues <- doImport (_dataData dt)  
      putStrLn $ show issues
    Left e -> putStrLn $ show e
