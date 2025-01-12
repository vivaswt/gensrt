{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, runStateT)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Environment (getArgs, getProgName)
import System.FilePath (replaceExtension)

-- $setup
-- >>> :set -XOverloadedStrings

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

-- | The main function of the program.
main :: IO ()
main = do
  result <- runExceptT generate
  case result of
    Left err -> TIO.putStrLn err
    Right msg -> TIO.putStrLn msg

-- | Generate an SRT file from a subtitle file and a word timestamp file.
generate :: ExceptT T.Text IO T.Text
generate = do
  (subtitleFileName, wordTimestampFileName) <- getInputFileNames

  subtitleLines <- readSubtitleFile subtitleFileName
  wordTimestamps <- readWordTimestampFile wordTimestampFileName

  result <- evalStateT (collateSubsWithTimestamps subtitleLines) wordTimestamps

  let outputFileName = getOutputFileName subtitleFileName
  let srtTexts = concat . zipWith segmentToSrtRow [1 ..] $ mergeSegments <$> result
  safeWriteFile outputFileName (T.unlines srtTexts)

  return $ "Generated " <> T.pack outputFileName

-- | Get the output file name from the subtitle file name.
getOutputFileName :: T.Text -> String
getOutputFileName subtitleFileName = replaceExtension fileName "srt"
  where
    fileName = T.unpack subtitleFileName

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

-- | Collate the subtitle file with the word timestamp file.
collateSubsWithTimestamps ::
  [[T.Text]] -> StateT [Segment] (ExceptT T.Text IO) [[Segment]]
collateSubsWithTimestamps subtitleLines = do
  r <- mapM collateSubtitleLine subtitleLines
  s <- get
  case s of
    [] -> return r
    _ -> lift . throwE $ "Unmatchted word: " <> segmentText (head s) <> " is not found in the subtitle file."

-- | Collate the line in the subtitle file with the word in the word timestamp file.
collateSubtitleLine :: [T.Text] -> StateT [Segment] (ExceptT T.Text IO) [Segment]
collateSubtitleLine = mapM collateWord

-- | Collate the word in the subtitle file with the word in the word timestamp file.
-- If the word in the subtitle file is not found in the word timestamp file,
-- an exception will be thrown.
-- Or if both words are not matched, an exception will be thrown.
--
-- >>> runExceptT $ runStateT (collateWord "word") [Segment 0 1 "word", Segment 1 2 "word2"]
-- Right (Segment {segmentStart = 0s, segmentEnd = 1s, segmentText = "word"},[Segment {segmentStart = 1s, segmentEnd = 2s, segmentText = "word2"}])
--
-- >>> runExceptT $ runStateT (collateWord "word") [Segment 0 1 "word2", Segment 1 2 "word"]
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

-- | Write a text to a file safely
-- If the file cannot be written, an error message is returned.
safeWriteFile :: FilePath -> T.Text -> ExceptT T.Text IO ()
safeWriteFile fileName content = do
  catch (lift $ TIO.writeFile fileName content) $
    \(SomeException e) -> throwE $ "Fail to write the file " <> T.pack (show e)

-- | Merge a list of Segments into a single Segment
-- The start time of the merged segment is the start time of the first segment.
-- The end time of the merged segment is the end time of the last segment.
-- The text of the merged segment is the concatenation of the text of all segments.
mergeSegments :: [Segment] -> Segment
mergeSegments [] = error "Empty list of Segments cann't be merged"
mergeSegments segments =
  Segment
    { segmentStart = segmentStart . head $ segments,
      segmentEnd = segmentEnd . last $ segments,
      segmentText = T.unwords . map segmentText $ segments
    }

-- | Convert a Segment to a list of SRT rows
-- The SRT row consists of four lines:
--   ie, the segment number, the start and end times, and the text of the segment.
-- Warning:
--  An empty line is not added at the end of the SRT rows.
--  Because if it is added, it will be repeated at the end of the file.
segmentToSrtRow :: Int -> Segment -> [T.Text]
segmentToSrtRow
  i
  Segment {segmentStart = start, segmentEnd = end, segmentText = text} =
    [ T.pack . show $ i,
      ftime start <> " --> " <> ftime end,
      text
    ]
    where
      -- \| Format a NominalDiffTime as a string in the SRT format
      ftime =
        T.pack
          . map (\c -> if c == '.' then ',' else c)
          . formatTime defaultTimeLocale "%0H:%0M:%03ES"