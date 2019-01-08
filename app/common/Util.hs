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
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import System.IO
import Text.RegexPR
import Text.Printf
import Debug.Trace
import Data.Monoid

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

listTags = intercalate " " . map (\t -> '%' : t : []) . map fst

gr_tags = [
    ('i', show . gid_id . gr_gid)
  , ('m', maybe "?" show . gr_members_count)
  , ('n', namefilter . Text.unpack . gr_name)
  , ('s', namefilter . Text.unpack . gr_screen_name)
  , ('u', Text.unpack . aurl_str . groupURL)
  ]

gr_format :: String -> GroupRecord -> String
gr_format s mr = pformat '%' gr_tags s mr

mr_full_id MusicRecord{..} = show mr_owner_id <> "_" <> show mr_id

mr_tags = [
    ('i', mr_full_id)
  , ('o', show . mr_owner_id)
  , ('a', namefilter . mr_artist)
  , ('t', namefilter . mr_title)
  , ('d', show . mr_duration)
  , ('u', mr_url_str)
  , ('U', cutextra . mr_url_str)
  ]

mr_format :: String -> MusicRecord -> String
mr_format s mr = pformat '%' mr_tags s mr

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

io :: (MonadIO m) => IO a -> m a
io = liftIO

printio, printerr :: (MonadIO m) => String -> m ()
printio = liftIO . hPutStrLn stdout
printerr = liftIO . hPutStrLn stderr

