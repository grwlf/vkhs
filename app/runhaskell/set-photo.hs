#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

import Prelude ()
import Web.VKHS
import Web.VKHS.Imports

main :: IO ()
main = runVK_ defaultOptions $ do
  u <- getCurrentUser
  setUserPhoto u "./data/heart.jpg"
