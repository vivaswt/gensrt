{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  result <- runExceptT generate
  case result of
    Left err -> TIO.putStrLn err
    _ -> TIO.putStrLn "completed"

generate :: ExceptT T.Text IO ()
generate = do
  (subtitleFileName, wordTimestampFileName) <- getInputFileNames
  lift . TIO.putStrLn $ "Subtitle file: " <> subtitleFileName
  lift . TIO.putStrLn $ "Word timestamp file: " <> wordTimestampFileName

-- | Get input file names from command line arguments.
-- The first argument is the subtitle file name
-- and the second argument is the word timestamp file name.
getInputFileNames :: ExceptT T.Text IO (T.Text, T.Text)
getInputFileNames = do
  args <- lift getArgs
  case args of
    [subtitleFileName, wordTimestampFileName] ->
      return (T.pack subtitleFileName, T.pack wordTimestampFileName)
    _ -> do
      pname <- lift $ T.pack <$> getProgName
      throwE $ "Usage: " <> pname <> " <subtitle file> <word timestamp file>"
