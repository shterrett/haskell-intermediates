module Main where

import JL.PrettyPrint (prettyPrint)
import JL.Focus.Parse (formatFocus, parseFocus)
import JL.Focus (focusOn)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  input <- BS.getContents
  json :: A.Value <- case A.eitherDecode input of
                        Left e -> do
                          putStrLn e
                          exitFailure
                        Right j -> pure j
  args <- getArgs
  focusString <- case args of
                    [s] -> pure $ Just s
                    [] -> pure Nothing
                    _ -> do
                      putStrLn $ "Provide zero or one argument"
                      exitFailure
  focusedJson <- case focusString of
                    Nothing -> pure $ Right ([], json)
                    Just fs -> case parseFocus (T.pack fs) of
                                Left e -> do
                                  putStrLn $ T.unpack e
                                  exitFailure
                                Right foci -> pure (focusOn foci json)
  case focusedJson of
    Right (_, fj) -> putStrLn . T.unpack $ prettyPrint "  " fj
    Left (matched, fj) -> do
      putStrLn . T.unpack 
        $ "Only a partial match on the focus string was possible\n"
       <> "The matched prefix was: " <> foldMap formatFocus matched <> "\n"
       <> "and the matched object was:\n"
       <> prettyPrint "  " fj
