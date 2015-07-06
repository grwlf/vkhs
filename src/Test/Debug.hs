-- FIXME: in order debug to work
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Debug where

import Control.Monad
import Control.Monad.Trans

class Debug x where
    debug :: (MonadIO m) => x -> m ()

instance {-# OVERLAPPABLE #-} (Show x) => Debug x where
    debug = liftIO . putStrLn . show . show

instance {-# OVERLAPPING #-} Debug String where
    debug = liftIO . putStrLn . show

