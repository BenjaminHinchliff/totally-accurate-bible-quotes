{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import qualified Data.ByteString.UTF8 as BL
import qualified Data.Char as Char
import Data.Maybe
import qualified Data.Trie as Trie
import System.Environment
import System.IO

stringToByteLower :: String -> BL.ByteString
stringToByteLower = BL.fromString . map Char.toLower

applyVerse :: [String] -> [(BL.ByteString, String)]
applyVerse (n : w) = zip (map stringToByteLower w) (repeat n)

parseVerse :: String -> [(BL.ByteString, String)]
parseVerse = applyVerse . words

parseVerses :: String -> Trie.Trie String
parseVerses = Trie.fromList . concatMap parseVerse . lines

-- case-insensitive trie lookup converting from String to trie ByteStrings
trieStringLookup :: String -> (Trie.Trie String -> Maybe String)
trieStringLookup = Trie.lookup . stringToByteLower

-- search for verse containing word
findVerse :: String -> Trie.Trie String -> Maybe (String, String)
findVerse w trie = (w,) <$> trieStringLookup w trie

buildQuoteParts :: [(String, String)] -> (String, String)
buildQuoteParts = foldr1 (\(w, c) (sw, sc) -> (w ++ "..." ++ sw, c ++ ", " ++ sc))

buildQuote :: [(String, String)] -> String
buildQuote results = "\"" ++ words ++ "\"" ++ " (" ++ citations ++ ")."
  where
    (words, citations) = buildQuoteParts results

main :: IO ()
main = do
  hPutStrLn stderr "Building word lookup tree..."
  kjv <- readFile "kjv.txt"
  let trie = parseVerses kjv

  args <- getArgs
  let source = unwords args
  let srcWords = words source
  let results = mapMaybe (`findVerse` trie) srcWords
  putStrLn $ buildQuote results
