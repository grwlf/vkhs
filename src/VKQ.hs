{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- module Main where
module VKQ where

import Control.Exception (SomeException(..),catch,bracket)
import Options.Applicative
import System.Environment

import Web.VKHS
import Web.VKHS.Types

data CmdOptions
  = Login LoginOptions
  | Call CallOptions
  | Music MusicOptions
  | UserQ UserOptions
  | WallQ WallOptions
  deriving(Show)

data Options = Options {
    verb :: Verbosity
  , cmdOpts :: CmdOptions
  } deriving(Show)

loginOptions :: Parser CmdOptions
loginOptions = Login <$> (LoginOptions
  <$> (AppID <$> strOption (metavar "APPID" <> short 'a' <> value "3128877" <> help "Application ID, defaults to VKHS" ))
  <*> argument str (metavar "USER" <> help "User name or email")
  <*> argument str (metavar "PASS" <> help "User password"))

opts m =
  let access_token_flag = strOption (short 'a' <> m <> metavar "ACCESS_TOKEN" <>
        help "Access token. Honores VKQ_ACCESS_TOKEN environment variable")
  in Options
  <$> flag Normal Debug (long "verbose" <> help "Be verbose")
  <*> subparser (
    command "login" (info loginOptions
      ( progDesc "Login and print access token (also prints user_id and expiration time)" ))
    <> command "call" (info (Call <$> (CallOptions
      <$> access_token_flag
      <*> switch (long "preparse" <> short 'p' <> help "Preparse into Aeson format")
      <*> argument str (metavar "METHOD" <> help "Method name")
      <*> argument str (metavar "PARAMS" <> help "Method arguments, KEY=VALUE[,KEY2=VALUE2[,,,]]")))
      ( progDesc "Call VK API method" ))
    <> command "music" (info ( Music <$> (MusicOptions
      <$> access_token_flag
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
      <$> access_token_flag
      <*> strOption (long "query" <> short 'q' <> help "String to query")
      ))
      ( progDesc "Extract various user information"))
    <> command "wall" (info ( WallQ <$> (WallOptions
      <$> access_token_flag
      <*> strOption (long "id" <> short 'i' <> help "Owner id")
      ))
      ( progDesc "Extract wall information"))
    )

main :: IO ()
main = do
  m <- maybe (idm) (value) <$> lookupEnv "VKQ_ACCESS_TOKEN"
  execParser (info (helper <*> opts m) (fullDesc <> header "VKontakte social network tool")) >>= cmd
  `catch` (\(e::SomeException) -> do
    putStrLn $ (show e))


cmd :: Options -> IO ()

-- Login
cmd (Options v (Login lo)) = do
  runLogin lo
  return ()


