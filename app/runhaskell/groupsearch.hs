#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude ()
import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions $ do
  Sized cnt gs <- groupSearch "АльфаБанк"
  forM_ gs $ \GroupRecord{..} -> do
    liftIO $ putStrLn gr_name
