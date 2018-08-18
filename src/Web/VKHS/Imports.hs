-- | This module re-imports common declarations used across the VKHS library
module Web.VKHS.Imports (
    module Web.VKHS.Imports
  , module Control.Arrow
  , module Control.Category
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.State
  , module Control.Monad.Cont
  , module Control.Monad.Reader
  , module Control.Monad.Except
  , module Control.Monad.Writer
  , module Control.Exception
  , module Data.Aeson
  , module Data.ByteString.Char8
  , module Data.ByteString.Lazy
  , module Data.Monoid
  , module Data.Char
  , module Data.Text
  , module Data.Text.IO
  , module Data.List
  , module Data.Set
  , module Data.Function
  , module Data.Either
  , module Data.Maybe
  , module Data.Map
  , module Data.Typeable
  , module Data.Data
  , module Data.Scientific
  , module Debug.Trace
  , module Text.Printf
  , module Text.Show.Pretty
  , module Text.Read
  , module System.IO
  ) where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

import Control.Arrow ((***),(&&&))
import Control.Category ((>>>))
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
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
import Data.Map(Map)
import Data.Monoid((<>))
import Data.Function (on)
import Data.Text (Text)
import Data.Text.IO (putStrLn, hPutStrLn)
import Data.List (head, length, sortBy, (++))
import Data.Set (Set)
import Debug.Trace
import Text.Printf
import Text.Show.Pretty
import Text.Read (readMaybe)
import System.IO (stdout,stderr,Handle)

bspack :: ByteString -> Text
bspack = tpack . ByteString.unpack

tpack :: String -> Text
tpack = Text.pack

tunpack :: Text -> String
tunpack = Text.unpack

tshow :: (Show a) => a -> Text
tshow = tpack . show

tputStrLn :: MonadIO m => Text -> m ()
tputStrLn t = liftIO $ Data.Text.IO.putStrLn t

thPutStrLn :: MonadIO m => Handle -> Text -> m ()
thPutStrLn h t = liftIO $ Data.Text.IO.hPutStrLn h t

