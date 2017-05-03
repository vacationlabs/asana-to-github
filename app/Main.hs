module Main where

import Data.Aeson
import qualified Data.Csv as CSV
import Lib

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  contents <- BSL.readFile "issues.json"
  case (eitherDecode contents :: Either String Data) of
    Right dt -> do
      let csv = CSV.encode $ _dataData dt
      putStr $ show $ csv
      BSL.writeFile "output.csv" csv
    Left e -> putStrLn $ show e
