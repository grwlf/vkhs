#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens

import Web.VKHS
import Web.VKHS.Imports hiding (putStrLn)
import Data.HashMap.Strict(HashMap)
import Data.List(concat,nub,sortOn)
import Data.Set(Set)
import Control.Lens (Lens', Lens, makeLenses, (<%=), (%=), (%%=), (^.), zoom, set, view, over, use, uses, _1, _2, _3, _4, _5, _6)
import Data.Function.Flippers


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

data Cache = Cache {
    _c_gm  :: HashMap (Integer,GroupId) [UserId]
  , _c_u :: Set UserId
  } deriving(Show,Eq)

$(makeLenses ''Cache)

initialCache = Cache mempty mempty

getGroupMembersN_C :: (MonadState Cache (m (R m x)), MonadAPI m x s) => Integer -> GroupId -> API m x [UserId]
getGroupMembersN_C o gid = do
  c <- use c_gm
  case (o,gid) `HashMap.lookup` c of
    Just uids -> do
      tputStrLn $ "(getGroupMembersN_C uses cache for " <> tshow gid <> ", offset " <> tshow o <> ")"
      return uids
    Nothing -> do
      uids <- getGroupMembersN o gid
      c_gm %= HashMap.insert (o,gid) uids
      return uids

pprint :: (Show a) => a -> IO ()
pprint = putStrLn . ppShow
printVK x = liftIO . putStrLn =<< ppShow <$> (runVK defaultOptions{o_verbosity=Trace} x)
goVK x = either (fail . show) id  <$> (runVK defaultOptions{o_verbosity=Trace} x)
debugVK x = liftIO . putStrLn =<< ppShow <$> (runVK defaultOptions{o_verbosity=Debug} x)
members grps = nub . concat <$> mapM getGroupMembers [gr_gid x | x <- grps]


matchingMembers2 :: (MonadAPI m x s) => [Text] -> [Text] -> API m x [UserRecord]
matchingMembers2 ws1 ws2 = do
  gids1 <- nub . map gr_gid <$> matchingGroups ws1
  tputStrLn $ "Keywords " <> (tshow ws1) <> " returned " <> tshow (length gids1) <> " groups"
  gids2 <- nub . map gr_gid <$> matchingGroups ws2
  tputStrLn $ "Keywords " <> (tshow ws2) <> " returned " <> tshow (length gids2) <> " groups"
  ms1 <- nub . concat <$> mapM getGroupMembers gids1
  tputStrLn $ "Group set1 has " <> tshow (length ms1) <> " members"
  ms2 <- nub . concat <$> mapM getGroupMembers gids2
  tputStrLn $ "Group set2 has " <> tshow (length ms2) <> " members"
  us <- getUsers $ Set.toList (Set.fromList ms1 `Set.intersection` Set.fromList ms2)
  return us

matchingMembers :: (MonadState Cache (m (R m x)), MonadAPI m x s) => [Text] -> ([UserId] -> API m x ()) -> API m x ()
matchingMembers kws f = do
  forM_ kws $ \kw -> do
    grps <- groupSearch kw
    forM_ grps $ \grp -> do
      whileM_ 0 $ \i -> do
        uids <- getGroupMembersN_C i (gr_gid grp)
        f uids
        case length uids == 1000 of
          True -> return $ Just (i+1000)
          False -> return Nothing

compare2 :: (MonadState Cache (m (R m x)), MonadAPI m x s) => [Text] -> [Text] -> API m x ()
compare2 ws1 ws2 =
  let
    fname = tunpack $ "matching_" <> Text.intercalate "_" (Text.words $ Text.unwords $ ws1<>ws2) <> ".txt"
    tee s = do
      liftIO $ Text.appendFile fname (s<>"\n")
      tputStrLn s
  in do
  liftIO $ writeFile fname ""
  matchingMembers ws1 $ \(Set.fromList -> us1) -> do
  matchingMembers ws2 $ \(Set.fromList -> us2) -> do
  us <- pure (Set.intersection us1 us2)
  cus <- use c_u
  urs <- getUsers (Set.toList $ us`Set.difference`cus)
  forM_ urs $ \ur@UserRecord{..} -> do
    tee $ printUserBio ur
  c_u %= Set.union us


go2 :: IO ()
go2 = do
  flip evalStateT initialCache $ do
    printVK $ do
      compare2 ["Machine Learing", "Машинное обучение", "Neural network", "Tensorflow"] ["Haskell"]

go3 :: IO ()
go3 = do
  flip evalStateT initialCache $ do
    printVK $ do
      compare2 ["Machine Learing", "Машинное обучение", "Neural network", "Tensorflow"] ["Компилятор", "Compiler"]

go4 :: IO ()
go4 = do
  flip evalStateT initialCache $ do
    printVK $ do
      compare2 ["Machine Learing", "Машинное обучение", "Neural network", "Tensorflow"] ["C++"]

  -- gids1 <- nub . map gr_gid <$> matchingGroups ws1
  -- -- tputStrLn $ "Keywords " <> (tshow ws1) <> " returned " <> tshow (length gids1) <> " groups"
  -- ms1 <- nub . concat <$> mapM getGroupMembers gids1
  -- -- tputStrLn $ "Groups of " <> tshow ws1 <> " have " <> tshow (length ms1) <> " members"
  -- return ms1

getUsersUrls ids = do
  us <- getUsers ids
  return $ us`zip`(map userUrl ids)

-- initTestState = TestState 0 False "foo" Nothing

main :: IO ()
main = do
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


