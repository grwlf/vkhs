#!/usr/bin/env runhaskell

This simple script read the list of all countries from VK database and print it
to the stdout. We start from enableing RecordWildCard extension

> {-# LANGUAGE RecordWildCards #-}

Almost all required modules are re-imported by Web.VKHS.Imports module. Since
we rely on Text heavily, the Prelude may be disabled.

> import Prelude ()

This modules provide the public functionality of VKHS

> import Web.VKHS
> import Web.VKHS.Imports

The main program are run using with all default options. They inlude permission
to interact with user and caching the acess token into ./.vkhs-access-token
file.

> main :: IO ()
> main = runVK_ defaultOptions $ do
>   Sized cnt cs <- getCountries
>   forM_ cs $ \Country{..} -> do
>     liftIO $ putStrLn co_title
