{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Util where

import Data.Char
import Data.Maybe
import Data.List
import Data.Text(Text(..),pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment
import System.Exit
import System.IO
import Text.RegexPR
import Text.Printf

import Web.VKHS.Types
import Web.VKHS.API.Types

{-
 _   _ _   _ _
| | | | |_(_) |___
| | | | __| | / __|
| |_| | |_| | \__ \
 \___/ \__|_|_|___/

 -}

csv_quote :: Text -> Text
csv_quote x = "\"" `Text.append` (Text.replace "\"" "\"\"" x) `Text.append` "\""

pshow :: (Show a) => a -> Text
pshow = Text.pack . show

mr_format :: String -> MusicRecord -> String
mr_format s mr = pformat '%'
  [ ('i', show . mr_id)
  , ('o', show . mr_owner_id)
  , ('a', namefilter . mr_artist)
  , ('t', namefilter . mr_title)
  , ('d', show . mr_duration)
  , ('u', mr_url)
  , ('U', cutextra . mr_url)
  ] s mr

pformat :: Char -> [(Char, a->String)] -> String -> a -> String
pformat x d s a = reverse $ scan [] s where
  scan h (c:m:cs)
    | c == x = scan ((reverse $ fromMaybe (const $ c:m:[]) (lookup m d) $ a)++h) cs
    | otherwise = scan (c:h) (m:cs)
  scan h (c:[]) = c:h
  scan h [] = h


trim_space = gsubRegexPR "^ +| +$" ""
one_space = gsubRegexPR " +" " "
normal_letters = filter (\c -> or [ isAlphaNum c , c=='-', c=='_', c==' ', c=='&'])
html_amp = gsubRegexPR "&amp;" "&"
no_html = gsubRegexPR re "" where
  re = concat $ intersperse "|" [ "&[a-z]+;" , "&#[0-9]+;" ]

cutextra = gsubRegexPR "\\?extra=.*" ""

namefilter :: String -> String
namefilter = trim_space . one_space . normal_letters . no_html . html_amp

