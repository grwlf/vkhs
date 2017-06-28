-- | This module re-imports common declarations used across the VKHS
module Web.VKHS.Imports (
    module Web.VKHS.Imports
  , module Control.Arrow
  , module Control.Category
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Exception
  , module Data.Aeson
  , module Data.ByteString.Char8
  , module Data.ByteString.Lazy
  , module Data.Monoid
  , module Data.Char
  , module Data.Text
  , module Data.Text.IO
  , module Data.List
  , module Data.Function
  , module Data.Either
  , module Data.Maybe
  , module Data.Typeable
  , module Data.Data
  , module Data.Scientific
  , module Text.Printf
  , module Prelude
  , module Text.Show.Pretty
  , module Text.Read
  ) where

import Control.Arrow ((***),(&&&))
import Control.Category ((>>>))
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad
import Control.Monad.Trans
import Control.Exception (SomeException(..),try,catch,bracket)
import Data.Aeson ((.=), (.:), (.:?), (.!=), FromJSON)
import Data.Typeable
import Data.Data
import Data.Char
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict,toChunks)
import Data.Scientific (Scientific, FPFormat(..))
import Data.Either
import Data.Maybe
import Data.Monoid((<>))
import Data.Function (on)
import Data.Text (Text(..), pack, unpack)
import Data.Text.IO (putStrLn, hPutStrLn)
import Data.List (head, length, sortBy, (++))
import Prelude (error, Integer, FilePath, (==), (.), Show(..), String,
                ($), IO(..), Bool(..), compare, Ordering(..),
                Read(..))
import Text.Printf
import Text.Show.Pretty
import Text.Read (readMaybe)

tpack :: String -> Text
tpack = pack
tunpack :: Text -> String
tunpack = unpack

tshow :: (Show a) => a -> Text
tshow = tpack . show
