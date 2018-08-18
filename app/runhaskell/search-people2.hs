#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Control.Lens as Lens

import Web.VKHS
import Web.VKHS.Imports hiding (putStrLn)
import Data.List(concat,nub,sortOn)
import Data.Set(Set)
import Control.Lens (Lens', Lens, makeLenses, (<%=), (%=), (%%=), (^.), zoom, set, view, over, use, uses, _1, _2, _3, _4, _5, _6)


printGroups :: IO ()
printGroups = runVK_ defaultOptions $ do
  gs <- sortOn (fmap (0-) . gr_members_count) <$> groupSearch "mlcourse"
  forM_ gs $ \gr@GroupRecord{..} -> do
    tputStrLn $ tshow gr_gid <> " \"" <> gr_name <> "\" " <> tshow gr_members_count

printMembers :: IO ()
printMembers = runVK_ defaultOptions $ do
  us <- getGroupMembers (GroupId 312608)
  forM_ us $ \u -> do
    tputStrLn $ tshow (uid_id u)

-- Found
--
-- https://vk.com/id1283111
-- https://vk.com/id1743388
-- https://vk.com/id1983541
-- https://vk.com/id5658871
--
printMatching :: IO ()
printMatching = runVK_ defaultOptions $ do
  us1 <- (Set.fromList . map uid_id) <$> getGroupMembers (GroupId 312608) -- Haskell
  us2 <- (Set.fromList . map uid_id) <$> getGroupMembers (GroupId 158557357) -- MlCourse
  tputStrLn $ "Intersection :" <> (tshow $ Set.toList $ Set.intersection us1 us2)
  return ()


-- matchingGroups :: [String] -> IO [GroupId]
matchingGroups keyords = do
    List.concat <$> (
      forM keyords $ \kw-> groupSearch kw)

pprint :: (Show a) => a -> IO ()
pprint = putStrLn . ppShow
printVK x = putStrLn =<< ppShow <$> (runVK defaultOptions x)
goVK x = either (fail . show) id  <$> (runVK defaultOptions x)
members grps = nub . concat <$> mapM getGroupMembers [gr_gid x | x <- grps]


matchingMembers2 :: (MonadAPI m x s) => [Text] -> [Text] -> API m x [UserRecord]
matchingMembers2 ws1 ws2 = do
  gids1 <- nub . map gr_gid <$> matchingGroups ws1
  tputStrLn $ "Keywords " <> (tshow ws1) <> " returned " <> tshow (length gids1) <> " groups"
  gids2 <- nub . map gr_gid <$> matchingGroups ws2
  tputStrLn $ "Keywords " <> (tshow ws2) <> " returned " <> tshow (length gids2) <> " groups"
  ms1 <- nub . concat <$> mapM getGroupMembers gids1
  tputStrLn $ "Group set1 hase " <> tshow (length ms1) <> " members"
  ms2 <- nub . concat <$> mapM getGroupMembers gids2
  tputStrLn $ "Group set2 hase " <> tshow (length ms2) <> " members"
  us <- getUsers $ Set.toList (Set.fromList ms1 `Set.intersection` Set.fromList ms2)
  return us

matchingMembers :: (MonadAPI m x s) => [Text] -> API m x [UserId]
matchingMembers ws1 = do
  gids1 <- nub . map gr_gid <$> matchingGroups ws1
  tputStrLn $ "Keywords " <> (tshow ws1) <> " returned " <> tshow (length gids1) <> " groups"
  ms1 <- nub . concat <$> mapM getGroupMembers gids1
  tputStrLn $ "Groups of " <> tshow ws1 <> " have " <> tshow (length ms1) <> " members"
  return ms1

userUrl :: UserId -> Text
userUrl (UserId x) = "https://vk.com/id" <> tshow x

getUsersUrls ids = do
  us <- getUsers ids
  return $ us`zip`(map userUrl ids)

data TestState = TestState {
    _heroId     :: Integer
  , _heroName   :: Bool
  , _heroUserId :: String
  , _heroElo    :: Maybe Integer
  } deriving(Show,Eq)

$(makeLenses ''TestState)

initTestState = TestState 0 False "foo" Nothing

main :: IO ()
main = do
  flip evalStateT initTestState $ do
    runVK_ defaultOptions{ o_max_request_rate_per_sec = 1} $ do
      mlgroups <- matchingGroups ["Machine Learing", "Машинное обучение", "Neural network", "Tensorflow"]
      hsgroups <- matchingGroups ["Haskell"]
      forM_ mlgroups $ \mlg -> do
        forM_ hsgroups $ \hsg -> do
          tputStrLn $ "Comparing \"" <> (gr_name mlg) <> "\" with \"" <> (gr_name hsg) <> "\""
          us1 <- Set.fromList . map uid_id <$> getGroupMembers (gr_gid mlg)
          us2 <- Set.fromList . map uid_id <$> getGroupMembers (gr_gid hsg)
          forM_ (Set.toList $ Set.intersection us1 us2) $ \u ->
            tputStrLn $ "https://vk.com/id" <> tshow u


