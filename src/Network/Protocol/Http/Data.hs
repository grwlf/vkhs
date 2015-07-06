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
{-# LANGUAGE TemplateHaskell, TypeOperators, KindSignatures #-}
module Network.Protocol.Http.Data where

import Control.Category
import Data.Char
import Data.List
import Data.List.Split
import Data.Label
import Network.Protocol.Http.Status
import Network.Protocol.Uri
import Prelude hiding ((.), id, lookup, mod)

-- | List of HTTP request methods.

data Method =
    OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OTHER String
  deriving (Read, Show, Eq)

-- | HTTP protocol version.

data Version = Version {_major :: Int, _minor :: Int}
  deriving (Read, Show, Eq, Ord)

type Key   = String
type Value = String

-- | HTTP headers as mapping from keys to values.

newtype Headers = Headers { unHeaders :: [(Key, Value)] } -- order seems to matter
  deriving (Read, Show, Eq)

-- | Request specific part of HTTP messages.

data Request = Request  { __method :: Method, __uri :: String }
  deriving (Read, Show, Eq)

-- | Response specific part of HTTP messages.

data Response = Response { __status :: Status }
  deriving (Read, Show, Eq)

-- | An HTTP message. The message body is *not* included.

data Http a = Http
  { _headline :: a
  , _version  :: Version
  , _headers  :: Headers
  } deriving (Read, Show, Eq)

-- | All recognized method constructors as a list.

methods :: [Method]
methods = [OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT]

-- | Create HTTP 1.0 version.

http10 :: Version
http10 = Version 1 0

-- | Create HTTP 1.1 version.

http11 :: Version
http11 = Version 1 1

-- | Create an empty set of headers.

emptyHeaders :: Headers
emptyHeaders = Headers []

-- | Create an empty HTTP request message.

emptyRequest :: Http Request
emptyRequest = Http (Request GET "") http11 emptyHeaders

-- | Create an empty HTTP response message.

emptyResponse :: Http Response
emptyResponse = Http (Response OK) http11 emptyHeaders

$(mkLabels [''Version, ''Request, ''Response, ''Http])

-- | Label to access the method part of an HTTP request message.

method :: Http Request :-> Method
method = _method . headline

-- | Label to access the URI part of an HTTP request message.

uri :: Http Request :-> String
uri = _uri . headline

-- | Label to access the URI part of an HTTP request message and access it as a
-- true URI data type.

asUri :: Http Request :-> Uri
asUri = (Bij toUri showUri) `iso` uri

-- | Label to access the status part of an HTTP response message.

status :: Http Response :-> Status
status = _status . headline

-- | Normalize the capitalization of an HTTP header key.

normalizeHeader :: Key -> Key
normalizeHeader = intercalate "-" . map casing . splitOn "-"
  where
  casing ""     = ""
  casing (x:xs) = toUpper x : map toLower xs

-- | Generic label to access an HTTP header field by key. Returns first matching
-- field

header :: Key -> Http a :-> Maybe Value
header key = lens
  (lookup (normalizeHeader key) . unHeaders . get headers)
  (\x -> modify headers (Headers . alter (normalizeHeader key) x . unHeaders))
  where
  alter :: Eq a => a -> Maybe b -> [(a, b)] -> [(a, b)]
  alter k v []                      = maybe [] (\w -> (k, w):[]) v
  alter k v ((x, y):xs) | k == x    = maybe xs (\w -> (k, w):xs) v
                        | otherwise = (x, y) : alter k v xs

-- | Label to access HTTP header fields, return a list of matching fields

headerMany :: Key -> Http a :-> [Value]
headerMany key = lens
  (map snd . filter (key_is (normalizeHeader key)) . unHeaders . get headers)
  (\x -> modify headers (Headers . alter (normalizeHeader key) x . unHeaders))
  where
  alter :: Eq a => a -> [b] -> [(a, b)] -> [(a, b)]
  alter k []     []                      = []
  alter k []     ((x, y):xs) | k == x    =          alter k [] xs
                             | otherwise = (x, y) : alter k [] xs
  alter k (v:vs) []                      = (k, v) : alter k vs []
  alter k (v:vs) ((x, y):xs) | k == x    = (k, v) : alter k vs xs
                             | otherwise = (x, y) : alter k (v:vs) xs
  key_is k' (k,v) = k==k'

