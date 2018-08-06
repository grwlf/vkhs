#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions $ do
  Sized cnt gs <- groupSearch "Альфа банк"
  forM_ gs $ \GroupRecord{..} -> do
    liftIO $ tputStrLn gr_name
