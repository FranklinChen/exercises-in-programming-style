#!/usr/bin/env runhaskell

-- | Franklin Chen
-- http://franklinchen.com/

import qualified System.Environment as E
import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Function (on)
import qualified Data.Ord as Ord
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

type Count = Int

stopWordsPath :: FilePath
stopWordsPath = "../stop_words.txt"

-- | Parse a list of stop words into a set.
--
-- Example file had trailing comma and spaces.
--
-- Note: filter and map can be fused by compiler for one pass without
-- intermediate list.
parseStopWords :: Text
               -> Set Text
parseStopWords =
  Text.split (== ',')
  >>> map Text.strip
  >>> filter (Text.length >>> (/= 0))
  >>> Set.fromList

-- | Parse text into a list of words, throwing away the short ones.
--
-- Note: Text.words and Text.map can be fused.
parseWords :: Text -> [Text]
parseWords =
  Text.map (\c -> if Char.isAlpha c then c else ' ')
  >>> Text.words
  >>> filter (Text.length >>> (> 2))

-- | Create an order for sorting.
--
-- Decreasing by count but ascending by text.
decreasingOrdering :: (Text, Count) -> (Ord.Down Count, Text)
decreasingOrdering (word, count) = (Ord.Down count, word)

-- | Return word-count pairs sorted by 'decreasingOrdering'.
-- Note: filter and map can be fused.
analyzeDocument :: Set Text -> Text -> [(Text, Count)]
analyzeDocument stopWordSet =
  parseWords
  >>> map Text.toLower
  >>> filter (`Set.notMember` stopWordSet)
  >>> countWords
  >>> List.sortBy (compare `on` decreasingOrdering)
  >>> take 25

-- | Count the occurrences of each word in a word list.
countWords :: [Text] -> [(Text, Count)]
countWords =
  map (\word -> (word, 1))
  >>> Map.fromListWith (+)
  >>> Map.toList

printInfo :: (Text, Count) -> IO ()
printInfo (word, count) =
  putStrLn $ Text.unpack word ++ " - " ++ show count

getDocumentPath :: IO FilePath
getDocumentPath = do
  args <- E.getArgs
  case args of
    [path] -> return path
    _ -> error "Usage: tf-05 path"

main :: IO ()
main = do
  stopWordSet <- parseStopWords <$> TextIO.readFile stopWordsPath
  documentText <- getDocumentPath >>= TextIO.readFile
  analyzeDocument stopWordSet documentText `forM_` printInfo
