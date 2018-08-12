{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.HTML.TagSoup.Parsec 
   ( module Text.HTML.TagSoup
   , TagParser
   , TagParserSt
   , WholeTag 
   , tParse
   , tStParse
   , openTag 
   , maybeOpenTag
   , eitherOpenTag
   , notOpenTag
   , allOpenTags
   , wholeTag
   , maybeWholeTag
   , eitherWholeTag
   , allWholeTags
   , closeTag
   , maybeCloseTag
   , eitherCloseTag
   , notCloseTag
   , allCloseTags
   , maybeP
   , allP
   , eitherP
   )
   where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.HTML.TagSoup

import Text.StringLike

import Data.Maybe
import Data.Char

-- | A type represent the TagOpen, any inner tags , and the TagClose.
type WholeTag str = (Tag str, [Tag str] , Tag str)

-- | The Tag parser, using Tag as the token.
type TagParser str = GenParser (Tag str) ()

-- | A stateful tag parser
-- This is a new type alias to allow backwards compatibility with old code.
type TagParserSt str u = GenParser (Tag str) u

-- | Used to invoke parsing of Tags.
tParse :: (StringLike str, Show str) => TagParser str a -> [ Tag str ] -> a
tParse p ts =
   either ( error . show ) id $ parse p "tagsoup" ts

-- | Simply run a stateful tag parser
tStParse :: (StringLike str, Show str) => TagParserSt str st a -> st -> [ Tag str ] -> a
tStParse p state tos =
   either ( error . show ) id $ runParser p state "tagsoup" tos
      

-- Tag eater is the basic tag matcher, it increments the line number for each tag parsed.
tagEater matcher =
   tokenPrim show 
             ( \ oldSp _ _ -> do 
                  setSourceLine oldSp ( 1 + sourceLine oldSp )
             )
             matcher

-- make a string lowercase
lowercase :: StringLike s => s -> s
lowercase =
   fromString . map toLower . toString


-- | openTag matches a TagOpen with the given name.  It is not case sensitive.
openTag :: (StringLike str, Show str) => str -> TagParserSt str st (Tag str)
openTag soughtName =
   openTagMatch soughtName ( Just ) $ \ _ -> Nothing

-- | notOpenTag will match any tag which is not a TagOpen with the given name.  It is not case sensitive.
notOpenTag :: (StringLike str, Show str) => str -> TagParserSt str st (Tag str)
notOpenTag avoidName =
   openTagMatch avoidName ( \ _ -> Nothing ) Just

-- openTagMatch is the higher order function which will receive a TagOpen, and call match if it matches the soughtName, and noMatch if it doesn't
openTagMatch soughtName match noMatch =
   tagEater $ \ tag ->
                 case tag of
                    t@( TagOpen tname atrs ) ->
                       if lowercase tname == lowercase soughtName
                          then
                             match t
                          else
                             noMatch t
                    t ->
                       noMatch t

-- closeTagMatch is the higher order function which will receive a TagClose, and call match if it matches the soughtName and noMatch if it doesn't.
closeTagMatch soughtName match noMatch =
   tagEater $ \ tag ->
                 case tag of
                    t@( TagClose tname ) ->
                       if lowercase tname == lowercase soughtName
                          then
                             match t
                          else
                             noMatch t
                    t ->
                       noMatch t

-- | wholeTag matches a TagOpen with the given name,
-- then all intervening tags,
-- until it reaches a TagClose with the given name.
-- It is not case sensitive.
wholeTag :: (StringLike str, Show str) => str -> TagParserSt str st (WholeTag str)
wholeTag soughtName = do
   open <- openTag soughtName
   ts <- many $ notCloseTag soughtName
   close <- closeTag soughtName
   return ( open , ts , close )

-- | closeTag matches a TagClose with the given name.  It is not case sensitive.
closeTag :: (StringLike str, Show str) => str -> TagParserSt str st (Tag str)
closeTag soughtName =
   closeTagMatch soughtName ( Just ) $ \ _ -> Nothing

-- | notCloseTag will match any tag which is not a TagClose with the given name.  It is not case sensitive.
notCloseTag :: (StringLike str, Show str) => str -> TagParserSt str st (Tag str)
notCloseTag avoidName =
   closeTagMatch avoidName ( \ _ -> Nothing ) Just

-- | maybeOpenTag will return `Just` the tag if it gets a TagOpen with he given name,
-- It will return `Nothing` otherwise.
-- It is not case sensitive.
maybeOpenTag :: (StringLike str, Show str) => str -> TagParserSt str st ( Maybe (Tag str) )
maybeOpenTag =
   maybeP . openTag

-- | maybeCloseTag will return `Just` the tag if it gets a TagClose with he given name,
-- It will return `Nothing` otherwise.
-- It is not case sensitive.
maybeCloseTag :: (StringLike str, Show str) => str -> TagParserSt str st ( Maybe (Tag str) )
maybeCloseTag =
   maybeP . closeTag

-- | maybeWholeTag will return `Just` the tag if it gets a WholeTag with he given name,
-- It will return `Nothing` otherwise.
-- It is not case sensitive.
maybeWholeTag :: (StringLike str, Show str) => str -> TagParserSt str st ( Maybe (WholeTag str) )
maybeWholeTag =
   maybeP . wholeTag

-- | allOpenTags will return all TagOpen with the given name.
-- It is not case sensitive.
allOpenTags :: (StringLike str, Show str) => str -> TagParserSt str st [ Tag str ]
allOpenTags =
   allP . maybeOpenTag

-- | allCloseTags will return all TagClose with the given name.
-- It is not case sensitive.
allCloseTags :: (StringLike str, Show str) => str -> TagParserSt str st [ Tag str ]
allCloseTags =
   allP . maybeCloseTag

-- | allWholeTags will return all WholeTag with the given name.
-- It is not case sensitive.
allWholeTags :: (StringLike str, Show str) => str -> TagParserSt str st [ WholeTag str ]
allWholeTags =
   allP . maybeWholeTag

-- | eitherP takes a parser, and becomes its `Either` equivalent -- returning `Right` if it matches, and `Left` of anyToken if it doesn't.
eitherP :: Show tok => GenParser tok st a -> GenParser tok st ( Either tok a )
eitherP p = do
   try ( do t <- p
            return $ Right t
       ) <|> ( do t <- anyToken
                  return $ Left t
             )
-- | either a Right TagOpen or a Left arbitary tag.
eitherOpenTag :: (StringLike str, Show str) => str -> TagParserSt str st ( Either (Tag str) (Tag str) )
eitherOpenTag = 
   eitherP . openTag

-- | either a Right TagClose or a Left arbitary tag.
eitherCloseTag :: (StringLike str, Show str) => str -> TagParserSt str st ( Either (Tag str) (Tag str) )
eitherCloseTag =
   eitherP . closeTag

-- | either a Right WholeTag or a Left arbitary tag.
eitherWholeTag :: (StringLike str, Show str) => str -> TagParserSt str st ( Either (Tag str) (WholeTag str) )
eitherWholeTag = 
   eitherP . wholeTag

-- | allP takes a parser which returns  a `Maybe` value, and returns a list of matching tokens.
allP :: GenParser tok st ( Maybe a ) -> GenParser tok st [ a ]
allP p = do
   ts <- many p
   let ls = 
          catMaybes ts
   return ls

-- | maybeP takes a parser, and becomes its `Maybe` equivalent -- returning `Just` if it matches, and `Nothing` if it doesn't.
maybeP :: Show tok => GenParser tok st a -> GenParser tok st ( Maybe a )
maybeP p =
   try ( do t <- p
            return $ Just t
       ) <|> ( do _ <- anyToken
                  return Nothing
             )


