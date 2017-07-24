#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.VKHS
import Web.VKHS.Imports

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type WallDescriptor = (Int,Int)

post_id :: WallDescriptor
post_id = (-6222142,98078)

wallDesc :: WallRecord -> WallDescriptor
wallDesc WallRecord{..} = (wr_owner_id,wr_id)

printUrl :: (MonadIO m) => WallDescriptor -> m ()
printUrl (o,i) = tputStrLn $ "https://vk.com/im?w=wall" <> tshow o <> "_" <> tshow i

makeUrl :: WallDescriptor -> Text
makeUrl (o,i) = "https://vk.com/im?w=wall" <> tshow o <> "_" <> tshow i

whileM :: (Monad m) => m Bool -> m ()
whileM m = do
  x <- m
  case x of
    True -> return ()
    False -> whileM m

-- FIXME: return many invalid posts
main :: IO ()
main = runVK_ defaultOptions { o_verbose = False} $ do

  Just wr0 <- getWallById post_id

  flip execStateT (HashSet.empty, [post_id]) $ do

    whileM $ do

      (visited,frontier) <- get

      case frontier of
        [] -> return True {- break -}
        (pid:frontier1) -> do

          tputStrLn $ makeUrl pid
          mwr <- lift $ getWallById pid

          case mwr of
            Nothing -> do
              put (visited, frontier1)
              return True

            Just wr -> do
              wr1 <- map wallDesc <$> pure (wr_copy_history wr)
              wr2 <- map wallDesc . rr_items <$> lift (getWallReposts wr)

              frontier2 <- pure $
                HashSet.toList (HashSet.fromList (wr1 <> wr2) `HashSet.difference` visited)

              put ( HashSet.insert (wallDesc wr) visited
                  , frontier1 <> frontier2)

              return False


