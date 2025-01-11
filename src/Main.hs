{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (Except, ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime)
import System.Environment (getArgs, getProgName)

-- | A data structure representing a segment of a video.
data Segment = Segment
  { -- | The start time of the segment
    segmentStart :: NominalDiffTime,
    -- | The end time of the segment
    segmentEnd :: NominalDiffTime,
    -- | The text of the segment
    segmentText :: T.Text
  }
  deriving (Show, Eq, Ord)

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

readSubtitleFile :: T.Text -> ExceptT T.Text IO [[T.Text]]
readSubtitleFile fileName =
  map T.words . T.lines <$> safeReadFile (T.unpack fileName)

readWordTimestampFile :: T.Text -> ExceptT T.Text IO [Segment]
readWordTimestampFile fileName =
  parseWordTimestampData <$> safeReadFile (T.unpack fileName)

-- | Collate the word in the subtitle file with the word in the word timestamp file.
-- If the word in the subtitle file is not found in the word timestamp file,
-- an exception will be thrown.
-- Or if both words are not matched, an exception will be thrown.
--
-- >>> runExceptT $ runStateT (collateWord (T.pack "word")) [Segment 0 1 (T.pack "word"), Segment 1 2 (T.pack "word2")]
-- Right (Segment {segmentStart = 0s, segmentEnd = 1s, segmentText = "word"},[Segment {segmentStart = 1s, segmentEnd = 2s, segmentText = "word2"}])
--
-- >>> runExceptT $ runStateT (collateWord (T.pack "word")) [Segment 0 1 (T.pack "word2"), Segment 1 2 (T.pack "word")]
-- Left "Unmatchted word: word to word2."
collateWord :: T.Text -> StateT [Segment] (ExceptT T.Text IO) Segment
collateWord word = do
  segments <- get
  case segments of
    [] ->
      lift
        . throwE
        $ "Unmatchted word: " <> word <> " is not found in the word timestamp file."
    (s : ss) -> do
      put ss
      if segmentText s == word
        then return s
        else lift . throwE $ "Unmatchted word: " <> word <> " to " <> segmentText s <> "."

-- | Parse input data to a list of Segment
-- The input data is a tab-separated text file with three columns.
-- If the input data is not in the correct format, an exception will be thrown.
-- The third column may contain leading spaces, so they are removed.
parseWordTimestampData :: T.Text -> [Segment]
parseWordTimestampData inputData = do
  linetext <- T.lines inputData
  let (w1 : w2 : w3 : _) = take 3 . splitByTab $ linetext
  return $
    Segment
      (readAsDiffTime w1)
      (readAsDiffTime w2)
      (ltrim w3)
  where
    splitByTab = T.splitOn "\t"
    ltrim = T.dropWhile isSpace

-- | Read a text as a DiffTime
readAsDiffTime :: T.Text -> NominalDiffTime
readAsDiffTime = realToFrac . (read :: String -> Double) . T.unpack

-- | Read file safely
-- If the file does not exist, an error message is returned.
-- If the file exists, the contents of the file are returned.
safeReadFile :: FilePath -> ExceptT T.Text IO T.Text
safeReadFile fileName = do
  catch (lift $ TIO.readFile fileName) $
    \(SomeException e) -> throwE $ "Fail to open the file " <> T.pack (show e)