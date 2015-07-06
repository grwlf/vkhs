{-
Copyright (c) Sebastiaan Visser 2008

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
-}

{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Network.Protocol.Cookie
(

-- | For more information:
--   http://tools.ietf.org/html/rfc6265

-- * Cookie datatype.
  Cookie (Cookie)
, emptyShort
, empty
, cookie
, setCookie

, CookieShort (CookieShort)
, toShort

-- * Accessing cookies.
, name
, value
, comment
, commentURL
, discard
, domain
, maxAge
, expires
, path
, port
, secure
, version

-- * Collection of cookies.
, Cookies
, unCookies
, gather
, pickCookie
)
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad (join)
import Data.Label
import qualified Data.Label.Abstract as A
import Data.Maybe
import Data.Char
import Data.Monoid
import Safe
import Data.List
import Network.Protocol.Uri.Query
import qualified Data.Map as M

-- | The `Cookie` data type containg one key/value pair with all the
-- (potentially optional) meta-data.

data Cookie =
  Cookie
    { _name       :: String
    , _value      :: String
    , _comment    :: Maybe String
    , _commentURL :: Maybe String
    , _discard    :: Maybe String
    , _domain     :: Maybe String
    , _maxAge     :: Maybe Int
    , _expires    :: Maybe String
    , _path       :: Maybe String
    , _port       :: [Int]
    , _secure     :: Bool
    , _version    :: Int
    } deriving(Show, Read, Eq)

$(mkLabels [''Cookie])


-- | Short notation for cookies

data CookieShort =
  CookieShort
    { _s_name  :: String
    , _s_value   :: String
    } deriving (Show, Eq)

$(mkLabels [''CookieShort])

-- | Convert Cookie to CookieShort
toShort :: Cookie -> CookieShort
toShort c = CookieShort (get name c) (get value c)

emptyShort :: CookieShort
emptyShort = CookieShort "" ""

-- | Create an empty cookie.

empty :: Cookie
empty = Cookie "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] False 0

-- | Show a semicolon separated list of attribute/value pairs. Only meta pairs
-- with significant values will be pretty printed.

showSetCookie :: Cookie -> ShowS
showSetCookie c =
    pair (get name c) (get value c)
  . opt  "comment"    (get comment c)
  . opt  "commentURL" (get commentURL c)
  . opt  "discard"    (get discard c)
  . opt  "domain"     (get domain c)
  . opt  "maxAge"     (fmap show $ get maxAge c)
  . opt  "expires"    (get expires c)
  . opt  "path"       (get path c)
  . lst  "port"       (map show $ get port c)
  . bool "secure"     (get secure c)
  . opt  "version"    (optval $ get version c)
  where
    attr a       = showString a
    val v        = showString ("=" ++ v)
    end          = showString "; "
    single a     = attr a . end
    pair a v     = attr a . val v . end
    opt a        = maybe id (pair a)
    lst _ []     = id
    lst a xs     = pair a $ intercalate "," xs
    bool _ False = id
    bool a True  = single a
    optval 0     = Nothing
    optval i     = Just (show i)

(!$) a b = b $ a
infixr 0 !$

parseSetCookie :: String -> Cookie
parseSetCookie s =
  let p = fw (keyValues ";" "=") s
  in Cookie
    { _name       = p !$ headMay >>> fmap fst >>> fromMaybe ""
    , _value      = p !$ headMay >>> fmap snd >>> fromMaybe ""
    , _comment    = p !$ lookup "comment"
    , _commentURL = p !$ lookup "commentURL"
    , _discard    = p !$ lookup "discard"
    , _domain     = p !$ lookup "domain"
    , _maxAge     = p !$ lookup "maxAge" >>> fmap readMay >>> join
    , _expires    = p !$ lookup "expires"
    , _path       = p !$ lookup "path"
    , _port       = p !$ lookup "port" >>> maybe [] (readDef [-1])
    , _secure     = p !$ lookup "secure" >>> maybe False (const True)
    , _version    = p !$ lookup "version" >>> maybe 1 (readDef 1)
    }

showCookie :: CookieShort -> String
showCookie c = _s_name c ++ "=" ++ _s_value c

parseCookie :: String -> CookieShort
parseCookie s =
  let p = fw (values "=") s
  in emptyShort
       { _s_name  = atDef "" p 0
       , _s_value = atDef "" p 1
       }

-- | Cookie parser and pretty printer as a lens. To be used in combination with
-- the /Set-Cookie/ header field.

setCookie :: Bijection (->) String Cookie
setCookie = Bij parseSetCookie (flip showSetCookie "")

-- cookieShort :: Bijection (->) String [CookieShort]
-- cookieShort = Bij parseCookie showCookie

-- | Cookie parser and pretty printer as a lens. To be used in combination with
-- the /Cookie/ header field.

cookie :: Bijection (->) String [CookieShort]
cookie = (A.liftBij $ Bij parseCookie showCookie) . values ";"

-- | A collection of multiple cookies. These can all be set in one single HTTP
-- /Set-Cookie/ header field.

data Cookies = Cookies { _unCookies :: M.Map String Cookie }
  deriving (Eq, Show, Read)

emptyC = Cookies M.empty

instance Monoid Cookies where
    mempty = emptyC
    mappend (Cookies a) (Cookies b) = Cookies (a`mappend`b)

$(mkLabels [''Cookies])

-- | Case-insensitive way of getting a cookie out of a collection by name.

pickCookie :: String -> Cookies :-> Maybe Cookie
pickCookie n = lookupL (map toLower n) . unCookies where
    lookupL k = lens (M.lookup k) (flip M.alter k . const)

-- | Convert a list to a cookies collection.

fromList :: [Cookie] -> Cookies
fromList = Cookies . M.fromList . map (\a -> (map toLower $ get name a, a))

-- | Get the cookies as a list.

toList :: Cookies -> [Cookie]
toList = map snd . M.toList . get unCookies

-- | Converts to/from cookie collection using a list

gather :: Bijection (->) [Cookie] Cookies
gather = Bij fromList toList

