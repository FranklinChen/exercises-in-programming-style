-- Franklin Chen
-- http://franklinchen.com/

import qualified System.Environment as E
import Control.Monad (liftM)
import Data.Char (isAlpha)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as IO

stopWordsPath :: FilePath
stopWordsPath = "../stop_words.txt"

-- Example file had trailing comma and spaces.
-- Note: filter and map can be fused by compiler for one pass without
-- intermediate list.
parseStopWords :: T.Text -> S.Set T.Text
parseStopWords =
  S.fromList .
  filter ((/= 0) . T.length) .
  map T.strip .
  T.split (== ',')

-- Note: T.words and T.map can be auto- fused.
parseWords :: T.Text -> [T.Text]
parseWords = T.words .
             T.map (\c -> if isAlpha c then c else ' ')

-- Return pairs sorted by decreasing count and word.
-- Note: sortBy and map can be fused; filter and map can be fused.
analyzeDocument :: S.Set T.Text -> T.Text -> [(Int, T.Text)]
analyzeDocument stopWordSet =
  L.sortBy (flip compare) .
  map (\(word, count) -> (count, word)) .
  countWords .
  filter (`S.notMember` stopWordSet) .
  map T.toLower .
  parseWords

countWords :: [T.Text] -> [(T.Text, Int)]
countWords =
  M.toList .
  M.fromListWith (+) .
  map (\word -> (word, 1))

printInfo :: (Int, T.Text) -> IO ()
printInfo (count, word) = putStrLn $ T.unpack word ++ " - " ++ show count

getDocumentPath :: IO FilePath
getDocumentPath = do
  args <- E.getArgs
  case args of
    [path] -> return path
    _ -> error "Usage: tf-05 path"

main :: IO ()
main = do
  stopWordSet <- liftM parseStopWords $ IO.readFile stopWordsPath
  documentText <- getDocumentPath >>=
                  IO.readFile
  mapM_ printInfo $ analyzeDocument stopWordSet documentText
