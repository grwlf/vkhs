{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (SomeException(..),catch,bracket)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Maybe
import Data.List
import Data.Char
import Data.Text(Text(..),pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import Text.RegexPR
import Text.Printf

import Web.VKHS
import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.API as API
import Web.VKHS.API.Types as API

data Options
  = Login LoginOptions
  | API APIOptions
  | Music MusicOptions
  | UserQ UserOptions
  | WallQ WallOptions
  | GroupQ GroupOptions
  deriving(Show)

genericOptions :: Parser GenericOptions
genericOptions = GenericOptions
  <$> (pure $ o_login_host defaultOptions)
  <*> (pure $ o_api_host defaultOptions)
  <*> (pure $ o_port defaultOptions)
  <*> flag False True (long "verbose" <> help "Be verbose")
  <*> (pure $ o_use_https defaultOptions)
  <*> fmap read (strOption (value (show $ o_max_request_rate_per_sec defaultOptions) <> long "req-per-sec" <> metavar "N" <> help "Max number of requests per second"))
  <*> flag True False (long "interactive" <> help "Allow interactive queries")

loginOptions :: Parser LoginOptions
loginOptions = LoginOptions
  <$> genericOptions
  <*> (AppID <$> strOption (long "appid" <> metavar "APPID" <> value "3128877" <> help "Application ID, defaults to VKHS" ))
  <*> argument str (metavar "USER" <> help "User name or email")
  <*> argument str (metavar "PASS" <> help "User password")

loginOptions' :: Parser LoginOptions
loginOptions' = LoginOptions
  <$> genericOptions
  <*> (AppID <$> strOption (long "appid" <> metavar "APPID" <> value "3128877" <> help "Application ID, defaults to VKHS" ))
  <*> strOption (long "user" <> value "" <> metavar "STR" <> help "User name or email")
  <*> strOption (long "pass" <> value "" <> metavar "STR" <> help "Password")

opts m =
  let access_token_flag = strOption (short 'a' <> m <> metavar "ACCESS_TOKEN" <>
        help "Access token. Honores VKQ_ACCESS_TOKEN environment variable")
      api_cmd = (info (API <$> (APIOptions
        <$> loginOptions'
        <*> access_token_flag
        <*> switch (long "preparse" <> short 'p' <> help "Preparse into Aeson format")
        <*> argument str (metavar "METHOD" <> help "Method name")
        <*> argument str (metavar "PARAMS" <> help "Method arguments, KEY=VALUE[,KEY2=VALUE2[,,,]]")))
        ( progDesc "Call VK API method" ))
  in subparser (
    command "login" (info (Login <$> loginOptions)
      ( progDesc "Login and print access token (also prints user_id and expiration time)" ))
    <> command "call" api_cmd
    <> command "api" api_cmd
    <> command "music" (info ( Music <$> (MusicOptions
      <$> loginOptions'
      <*> access_token_flag
      <*> switch (long "list" <> short 'l' <> help "List music files")
      <*> strOption
        ( metavar "STR"
        <> long "query" <> short 'q' <> value [] <> help "Query string")
      <*> strOption
        ( metavar "FORMAT"
        <> short 'f'
        <> value "%o_%i %U\t%t"
        <> help "Listing format, supported tags: %i %o %a %t %d %u"
        )
      <*> strOption
        ( metavar "FORMAT"
        <> short 'F'
        <> value "%o_%i %U\t%t"
        <> help "FileName format, supported tags: %i %o %a %t %d %u"
        )
      <*> strOption (metavar "DIR" <> short 'o' <> help "Output directory" <> value "")
      <*> many (argument str (metavar "RECORD_ID" <> help "Download records"))
      <*> flag False True (long "skip-existing" <> help "Don't download existing files")
      ))
      ( progDesc "List or download music files"))
    <> command "user" (info ( UserQ <$> (UserOptions
      <$> loginOptions'
      <*> access_token_flag
      <*> strOption (long "query" <> short 'q' <> help "String to query")
      ))
      ( progDesc "Extract various user information"))
    <> command "wall" (info ( WallQ <$> (WallOptions
      <$> loginOptions'
      <*> access_token_flag
      <*> strOption (long "id" <> short 'i' <> help "Owner id")
      ))
      ( progDesc "Extract wall information"))
    <> command "group" (info ( GroupQ <$> (GroupOptions
      <$> loginOptions'
      <*> access_token_flag
      <*> strOption (long "query" <> short 'q' <> value [] <> help "Group search string")
      ))
      ( progDesc "Extract groups information"))
    )

main :: IO ()
main = ( do
  m <- maybe (value "") (value) <$> lookupEnv "VKQ_ACCESS_TOKEN"
  o <- execParser (info (helper <*> opts m) (fullDesc <> header "VKontakte social network tool"))
  r <- runEitherT (cmd o)
  case r of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right _ -> do
      return ()
  )`catch` (\(e::SomeException) -> do
    putStrLn $ (show e)
    exitFailure
  )

{-
  ____ _     ___
 / ___| |   |_ _|
| |   | |    | |
| |___| |___ | |
 \____|_____|___|

 -}

cmd :: Options -> EitherT String IO ()

-- Login
cmd (Login lo) = do
  AccessToken{..} <- runLogin lo
  liftIO $ putStrLn at_access_token

-- API
cmd (API (APIOptions{..})) = do
  runAPI a_login_options a_access_token (api a_method (splitFragments "," "=" a_args))
  return ()

-- Query audio files
cmd (Music (MusicOptions{..}))

  |not (null m_search_string) = do
    runAPI m_login_options m_access_token $ do
      API.Response (SizedList len ms) <- apiG "audio.search" [("q",m_search_string)]
      forM_ ms $ \m -> do
        liftIO $ printf "%s\n" (mr_format m_output_format m)
      liftIO $ printf "total %d\n" len

  |m_list_music = do
    runAPI m_login_options m_access_token $ do
      (API.Response (ms :: [MusicRecord])) <- apiG "audio.get" [("q",m_search_string)]
      forM_ ms $ \m -> do
        liftIO $ printf "%s\n" (mr_format m_output_format m)

-- Query groups files
cmd (GroupQ (GroupOptions{..}))

  |not (null g_search_string) = do
    runAPI g_login_options g_access_token $ do

      let kws = ["Handmade", "вязание", "knit"]
      forM_ kws $ \kw -> do

        (JSON{..}, API.Response (Result cnt (ms :: [GroupRecord]))) <- apiCombined "groups.search"
              [("q",kw),
               ("v","5.44"),
               ("fields", "can_post,members_count"),
               ("count", "1000")]

        -- liftIO $ putStrLn (show js_aeson)

        forM_ ([0..]`zip`ms) $ \(i,g@GroupRecord{..}) -> do
          when (  gr_can_post == Just True
               && gr_is_closed == GroupOpen) $ do
            liftIO $ Text.hPutStrLn stderr $ Text.intercalate "," [
                pack kw
              , pshow (fromJust gr_members_count)
              , pack (groupURL g)
              , csv_quote gr_name
              ]

          return ()

      -- forM_ ms $ \m -> do
      --   liftIO $ printf "%s\n" (mr_format g_output_format m)
      -- liftIO $ printf "total %d\n" len

{-
 _   _ _   _ _
| | | | |_(_) |___
| | | | __| | / __|
| |_| | |_| | \__ \
 \___/ \__|_|_|___/

 -}

csv_quote :: Text -> Text
csv_quote x = "\"" `Text.append` (Text.replace "\"" "\"\"" x) `Text.append` "\""

pshow :: (Show a) => a -> Text
pshow = Text.pack . show

mr_format :: String -> MusicRecord -> String
mr_format s mr = pformat '%'
  [ ('i', show . mr_id)
  , ('o', show . mr_owner_id)
  , ('a', namefilter . mr_artist)
  , ('t', namefilter . mr_title)
  , ('d', show . mr_duration)
  , ('u', mr_url)
  , ('U', cutextra . mr_url)
  ] s mr

pformat :: Char -> [(Char, a->String)] -> String -> a -> String
pformat x d s a = reverse $ scan [] s where
  scan h (c:m:cs)
    | c == x = scan ((reverse $ fromMaybe (const $ c:m:[]) (lookup m d) $ a)++h) cs
    | otherwise = scan (c:h) (m:cs)
  scan h (c:[]) = c:h
  scan h [] = h


trim_space = gsubRegexPR "^ +| +$" ""
one_space = gsubRegexPR " +" " "
normal_letters = filter (\c -> or [ isAlphaNum c , c=='-', c=='_', c==' ', c=='&'])
html_amp = gsubRegexPR "&amp;" "&"
no_html = gsubRegexPR re "" where
  re = concat $ intersperse "|" [ "&[a-z]+;" , "&#[0-9]+;" ]

cutextra = gsubRegexPR "\\?extra=.*" ""

namefilter :: String -> String
namefilter = trim_space . one_space . normal_letters . no_html . html_amp

