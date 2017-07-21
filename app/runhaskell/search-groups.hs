#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

import Prelude ()
import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions $ do
  Sized cnt gs <- getGroups "Альфа банк"
  forM_ gs $ \Group{..} -> do
    liftIO $ putStrLn gr_name
