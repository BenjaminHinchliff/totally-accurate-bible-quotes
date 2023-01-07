{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString.UTF8 as BL
import qualified Data.Char as Char
import Data.Function
import qualified Data.HashMap.Lazy as HashMap
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as Trie
import System.Environment
import System.IO

stringToLower :: String -> String
stringToLower = map Char.toLower

stringClean :: String -> String
stringClean = filter Char.isAlpha . stringToLower

-- build array of tuples where each contains the source verse
applyVerse :: [String] -> [(BL.ByteString, [String])]
applyVerse (n : w) = zip (map (BL.fromString . stringClean) w) (repeat [n])

buildWords :: String -> [(BL.ByteString, [String])]
buildWords = applyVerse . words

type WordsMap = Trie.Trie [String]

buildWordsMap :: String -> WordsMap
buildWordsMap = Trie.fromListWith (++) . concatMap buildWords . lines

-- split a verse into number and words
splitVerse :: String -> (String, [String])
splitVerse v = (n, map stringClean w) where (n : w) = words v

type VersesMap = HashMap.HashMap String [String]

buildVersesMap :: String -> VersesMap
buildVersesMap = HashMap.fromList . map splitVerse . lines

-- case-insensitive trie lookup converting from String to trie ByteStrings
trieStringLookup :: String -> (WordsMap -> Maybe [String])
trieStringLookup = Trie.lookup . BL.fromString

-- search for verse containing word
-- findVerse :: String -> Trie.Trie String -> Maybe (String, String)
-- findVerse w trie = (w,) <$> trieStringLookup w trie

buildQuoteParts :: [Fragment] -> (String, String)
buildQuoteParts = foldr1 (\(w, c) (sw, sc) -> (w ++ "..." ++ sw, c ++ ", " ++ sc))

buildQuote :: [Fragment] -> String
buildQuote results = "\"" ++ words ++ "\"" ++ " (" ++ citations ++ ")."
  where
    (words, citations) = buildQuoteParts results

type Fragment = (String, String)

matchingWords :: [String] -> [String] -> Int
matchingWords (a : as) (b : bs) = if a == b then 1 + matchingWords as bs else 0
matchingWords _ _ = 0

fragmentLength :: [String] -> [String] -> Int
fragmentLength targets@(target : _) words = matchingWords targets $ dropWhile (/= target) words
fragmentLength _ _ = 0 -- empty target catch

greedyWordsFragments :: [String] -> WordsMap -> VersesMap -> Maybe [Fragment]
greedyWordsFragments [] _ _ = Just []
greedyWordsFragments targets wordsMap versesMap = do
  verses <- head targets `trieStringLookup` wordsMap
  let verseTexts = mapMaybe (\v -> (,v) <$> v `HashMap.lookup` versesMap) verses
  let frags = map (first (fragmentLength targets)) verseTexts
  let (l, bestVerse) = maximumBy (compare `on` fst) frags
  let best = take l targets
  nextFrags <- greedyWordsFragments (drop l targets) wordsMap versesMap
  Just ((unwords best, bestVerse) : nextFrags)

main :: IO ()
main = do
  kjv <- readFile "kjv.txt"
  let versesMap = buildVersesMap kjv
  let wordsMap = buildWordsMap kjv

  args <- getArgs
  -- join together args then split them into words
  let source = map stringClean $ words $ unwords args

  let fragments = greedyWordsFragments source wordsMap versesMap
  putStrLn $ maybe "Unable to build quote" buildQuote fragments
