{-
 -
 - Copyright (c) 2009-2010 Johnny Morrice
 -
 - Permission is hereby granted, free of charge, to any person
 - obtaining a copy of this software and associated documentation 
 - files (the "Software"), to deal in the Software without 
 - restriction, including without limitation the rights to use, copy, 
 - modify, merge, publish, distribute, sublicense, and/or sell copies 
 - of the Software, and to permit persons to whom the Software is 
 - furnished to do so, subject to the following conditions:
 -
 - The above copyright notice and this permission notice shall be 
 - included in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 - BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 - ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 - CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 - SOFTWARE.
 -
-}
module Network.Shpider.Forms 
   ( module Network.Shpider.Pairs 
   , Form (..)
   , Method (..)
   , gatherForms
   , fillOutForm
   , allForms
   , toForm
   , mkForm
   , gatherTitle
   , emptyInputs
   , gatherCaptcha
   )
   where

import Data.Maybe


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.String.UTF8 as U (UTF8)
import qualified Data.String.UTF8 as U

import qualified Data.Map as M

import Text.HTML.TagSoup.Parsec

import Network.Shpider.TextUtils
import Network.Shpider.Pairs
import Text.StringLike (StringLike(..))

-- | Either GET or POST.
data Method =
   GET | POST
   deriving (Show,Eq)

-- | Plain old form: Method, action and inputs.
data Form =
   Form { method :: Method
        , action :: String
        , inputs :: M.Map String String
        }
   deriving (Show,Eq)

emptyInputs :: Form -> [String]
emptyInputs = fst . unzip . filter ( not . null . snd )  . M.toList . inputs

-- | Takes a form and fills out the inputs with the given [ ( String , String ) ].
-- It is convienent to use the `pairs` syntax here.
--
-- @
-- f : _ <- `getFormsByAction` \"http:\/\/whatever.com\"
-- `sendForm` $ `fillOutForm` f $ `pairs` $ do
--    \"author\" =: \"Johnny\"
--    \"message\" =: \"Nice syntax dewd.\"
-- @
fillOutForm :: Form -> [ ( String , String ) ] -> Form
fillOutForm f is =
   foldl ( \ form ( n , v ) -> form { inputs = M.insert n v $ inputs form } )
         f
         is

-- | The first argument is the action attribute of the form, the second is the method attribute, and the third are the inputs.
mkForm :: String -> Method -> [ ( String , String ) ] -> Form
mkForm a m ps =
   Form { action = a
        , method = m
        , inputs = M.fromList ps
        }

-- | Gets all forms from a list of tags.
gatherForms :: [ Tag String ] -> [ Form ]
gatherForms =
   tParse allForms

gatherTitle :: [Tag String] -> String
gatherTitle ts = case tParse allTitles ts of { [] -> "" ; x:_ -> x }

gatherCaptcha :: [Tag String] -> Maybe String
gatherCaptcha ts = case tParse allCaptchas ts of { [] -> Nothing ; x:_ -> Just x }

allTitles :: TagParser String [String]
allTitles = do
   fs <- allWholeTags "title"
   return $ mapMaybe (
    \(TagOpen "title" _ , innerTags , _ ) ->
      return $ concat $ map (\t -> case t of
        TagText t2 -> t2
        _ -> []
        ) innerTags
    ) fs

allCaptchas :: TagParser String [String]
allCaptchas = do
  ts <- allWholeTags "img"
  return $ flip mapMaybe ts $
    \(TagOpen "img" attrs , _ , _ ) ->
      case (lookup "id" attrs, lookup "src" attrs) of
        (Just "captcha", Just url) -> Just url
        _ -> Nothing

-- | The `TagParser` which parses all forms.
allForms :: TagParser String [ Form ]
allForms = do
   fs <- allWholeTags "form"
   return $ mapMaybe toForm fs

toForm :: WholeTag String -> Maybe Form
toForm ( TagOpen _ attrs , innerTags , _ ) = do
   m <- methodLookup attrs
   a <- attrLookup "action" attrs
   let is = tParse ( allOpenTags "input" ) innerTags
       tas = tParse ( allWholeTags "textarea" ) innerTags
   Just $ Form { inputs = M.fromList $ mapMaybe inputNameValue is ++ mapMaybe textAreaNameValue tas
               , action = a
               , method = m
               }

toForm _ = Nothing

methodLookup :: [(String, String)] -> Maybe Method
methodLookup attrs = do
   m <- attrLookup "method" attrs
   case lowercase m of
      "get" ->
         Just GET
      "post" ->
         Just POST
      _ ->
         Nothing

inputNameValue :: Tag String -> Maybe (String, [Char])
inputNameValue ( TagOpen _ attrs ) = do
   v <- case attrLookup "value" attrs of
           Nothing ->
              Just ""
           j@(Just _ ) ->
              j
   n <- attrLookup "name" attrs
   Just ( n , v )

inputNameValue _ = Nothing

textAreaNameValue :: StringLike b => (Tag String, [Tag b], c) -> Maybe (String, b)
textAreaNameValue ( TagOpen _ attrs , inner , _ ) = do
   let v = innerText inner
   n <- attrLookup "name" attrs
   Just ( n , v )
textAreaNameValue _ = Nothing

