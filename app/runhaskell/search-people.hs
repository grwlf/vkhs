#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Text.IO as Text

import Web.VKHS
import Web.VKHS.Imports hiding (putStrLn)
import Data.List(sortOn)
import Data.Set(Set)

withChunked_ :: (MonadAPI m x s) => (Integer -> API m x [a]) -> ([a] -> API m x ()) -> API m x ()
withChunked_ reader consumer = do
  whileM_ 0 $ \i -> do
    pack <- reader i
    consumer pack
    case length pack == 1000 of
      True -> return $ Just (i+1000)
      False -> return Nothing

withChunked :: (MonadAPI m x s) => (Integer -> API m x [a]) -> ([a] -> API m x b) -> API m x [b]
withChunked reader consumer = do
  whileM 0 [] $ \i b -> do
    pack <- reader i
    b' <- consumer pack
    case length pack == 1000 of
      True -> return $ (Just (i+1000), b<>[b'])
      False -> return (Nothing, b<>[b'])

chunked :: (MonadAPI m x s) => (Integer -> API m x [a]) -> API m x [a]
chunked f = concat <$> withChunked f return

tee :: (MonadIO m) => FilePath -> Text -> m ()
tee fpath s = do
  liftIO $ Text.appendFile fpath (s<>"\n")
  tputStrLn s

printUserBioHtml :: UserRecord -> Text
printUserBioHtml UserRecord{..} =
  "<div>" <>
  "<p><img src='" <> maybe "NONE" printAPIUrl (msum [ur_photo_400_orig,ur_photo_max]) <> "'</img></p>" <>
  "<p><a href='" <> printUserUrl ur_uid <> "'>" <> tshow (uid_id ur_uid) <> "</a></p>" <>
  "<p>name \""<> ur_first_name <> " " <> ur_last_name <> "\" </p>" <>
  "<p>birth \"" <> (maybe "?" id ur_bdate) <> "\"</p>" <>
  "<p>city \"" <> (maybe "?" (c_title) ur_city) <> "\"</p>" <>
  "</div>\n"

main :: IO ()
main = let
    fname = "MaryList-12.html"
  in do
  runVK_ defaultOptions $ do
    users <-
      -- sortBy (compare `on` ur_uid) <$> do
        chunked (usersSearch defaultUsersSearchArgs{ usa_q = "Мария Глазкова" })

    liftIO $ do
      writeFile fname "<head/><body>"
      forM_ users $ \u -> do
        tputStrLn $ printUserBio u
        Text.appendFile fname $ printUserBioHtml u
      Text.appendFile fname "</body>"

