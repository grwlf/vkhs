#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude ()
import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions $ do
  Sized cnt gs <- groupSearch "АльфаБанк"
  forM_ gs $ \gr@GroupRecord{..} -> do
    liftIO $ putStrLn gr_name
    liftIO $ putStrLn "--------------"
    Sized wc ws <- getGroupWall gr
    forM_ ws $ \wr@WallRecord{..} -> do
      liftIO $ putStrLn wr_text
      liftIO $ putStrLn "--------------"
