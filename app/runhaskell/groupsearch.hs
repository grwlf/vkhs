#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude ()
import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions { o_verbose = True, o_max_request_rate_per_sec = 1.5 } $ do
  Sized cnt gs <- groupSearch "Битлз"
  forM_ gs $ \gr@GroupRecord{..} -> do
    liftIO $ putStrLn gr_name
    liftIO $ putStrLn "--------------"
    Sized wc ws <- getGroupWall gr
    forM_ ws $ \wr@WallRecord{..} -> do
      liftIO $ putStrLn $ "\t" <> wr_text
