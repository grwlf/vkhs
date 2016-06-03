#!/usr/bin/env runhaskell

import Web.VKHS
import Web.VKHS.Types
import Web.VKHS.Client as Client
import Web.VKHS.Monad hiding (catch)
import Web.VKHS.API as API

main :: IO ()
main = runAPI defaultOptions $ do
  Sized cnt cs <- getCountries
  forM_ cs $ \Country{..} -> do
    putStrLn co_title
