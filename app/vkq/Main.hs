{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (SomeException(..),catch,bracket)
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Options.Applicative
import System.Environment
import System.Exit
import System.IO

import Web.VKHS
import Web.VKHS.Types

data Options
  = Login LoginOptions
  | API APIOptions
  | Music MusicOptions
  | UserQ UserOptions
  | WallQ WallOptions
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
  <*> (AppID <$> strOption (metavar "APPID" <> short 'a' <> value "3128877" <> help "Application ID, defaults to VKHS" ))
  <*> argument str (metavar "USER" <> help "User name or email")
  <*> argument str (metavar "PASS" <> help "User password")

loginOptions' :: Parser LoginOptions
loginOptions' = LoginOptions
  <$> genericOptions
  <*> (AppID <$> strOption (metavar "APPID" <> short 'a' <> value "3128877" <> help "Application ID, defaults to VKHS" ))
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
        <> value "%o_%i %u\t%t"
        <> help "Listing format, supported tags: %i %o %a %t %d %u"
        )
      <*> strOption
        ( metavar "FORMAT"
        <> short 'F'
        <> value "%a - %t"
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
      exitSuccess
  )`catch` (\(e::SomeException) -> do
    putStrLn $ (show e)
    exitFailure
  )

cmd :: Options -> EitherT String IO ()

-- Login
cmd (Login lo) = do
  AccessToken{..} <- runLogin lo
  liftIO $ putStrLn at_access_token

-- API
cmd (API ao) = do
  runAPI ao
  return ()

-- Query audio files
-- cmd (Music (MusicOptions act _ q@(_:_) fmt _ _ _ _)) = do
--   let e = (envcall act) { verbose = v }
--   Response (SL len ms) <- api_ e "audio.search" [("q",q)]
--   forM_ ms $ \m -> do
--     printf "%s\n" (mr_format fmt m)
--   printf "total %d\n" len

