{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Text.Namefilter
  ( namefilter
  ) where

import Data.Char
import Data.List
import Text.RegexPR

trim_space = gsubRegexPR "^ +| +$" ""
one_space = gsubRegexPR " +" " "
normal_letters = filter (\c -> or [ isAlphaNum c , c=='-', c=='_', c==' ', c=='&'])
html_amp = gsubRegexPR "&amp;" "&"
no_html = gsubRegexPR re "" where
  re = concat $ intersperse "|" [ "&[a-z]+;" , "&#[0-9]+;" ]

namefilter :: String -> String
namefilter = trim_space . one_space . normal_letters . no_html . html_amp
