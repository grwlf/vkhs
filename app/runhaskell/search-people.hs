#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as List

import Web.VKHS
import Web.VKHS.Imports hiding (putStrLn)
import Data.List(sortOn)
import Data.Set(Set)

printGroups = runVK_ defaultOptions $ do
  Sized cnt gs <- fmap (sortOn (fmap (0-) . gr_members_count)) <$> groupSearch "mlcourse"
  forM_ gs $ \gr@GroupRecord{..} -> do
    tputStrLn $ tshow gr_gid <> " \"" <> gr_name <> "\" " <> tshow gr_members_count

printMembers = runVK_ defaultOptions $ do
  Sized cnt us <- getGroupMembers (GroupId 312608)
  forM_ us $ \u -> do
    tputStrLn $ tshow (uid_id u)

-- Found
--
-- https://vk.com/id1283111
-- https://vk.com/id1743388
-- https://vk.com/id1983541
-- https://vk.com/id5658871
--
printMatching = runVK_ defaultOptions $ do
  Sized _ us1 <- fmap (Set.fromList . map uid_id) <$> getGroupMembers (GroupId 312608) -- Haskell
  Sized _ us2 <- fmap (Set.fromList . map uid_id) <$> getGroupMembers (GroupId 158557357) -- MlCourse
  tputStrLn $ "Intersection :" <> (tshow $ Set.toList $ Set.intersection us1 us2)
  return ()


-- matchingGroups :: [String] -> IO [GroupId]
matchingGroups kws = do
    List.concat <$> (
      forM kws $ \kw-> do
        m_items <$> groupSearch kw)

main :: IO ()
main = do
  runVK_ defaultOptions{ o_max_request_rate_per_sec = 1} $ do
    mlgroups <- matchingGroups ["Machine Learing", "Машинное обучение", "Neural network", "Tensorflow"]
    hsgroups <- matchingGroups ["Haskell"]
    forM_ mlgroups $ \mlg -> do
      forM_ hsgroups $ \hsg -> do
        tputStrLn $ "Comparing \"" <> (gr_name mlg) <> "\" with \"" <> (gr_name hsg) <> "\""
        Sized _ us1 <- fmap (Set.fromList . map uid_id) <$> getGroupMembers (gr_gid mlg)
        Sized _ us2 <- fmap (Set.fromList . map uid_id) <$> getGroupMembers (gr_gid hsg)
        forM_ (Set.toList $ Set.intersection us1 us2) $ \u ->
          tputStrLn $ "https://vk.com/id" <> tshow u


