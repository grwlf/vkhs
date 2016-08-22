#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

import Prelude ()
import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions $ do
  Sized cnt cs <- getCountries
  forM_ cs $ \Country{..} -> do
    liftIO $ putStrLn co_title
