#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions { o_verbosity = Normal, o_max_request_rate_per_sec = 1.5 } $ do
  Sized cnt gs <- groupSearch "Битлз"
  forM_ gs $ \gr@GroupRecord{..} -> do
    liftIO $ tputStrLn gr_name
    liftIO $ tputStrLn "--------------"
    Sized wc ws <- getGroupWall gr
    forM_ ws $ \wr@WallRecord{..} -> do
      liftIO $ tputStrLn $ "\t" <> wr_text
