module Main where

import JL.PrettyPrint

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  f <- BS.getContents
  case A.eitherDecode f of
    Left e -> do
      putStrLn e
      exitFailure
    Right json -> do
      putStrLn $ T.unpack $ prettyPrint "  " json
      exitSuccess
