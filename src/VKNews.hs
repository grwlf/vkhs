{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Either
import Data.Monoid
import Data.Time.Clock
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.RegexPR
import Web.VKHS as VK hiding (api,api')
import Web.VKHS.API.Monad as VK
import Network.CURL730

data Options = Options
  { verb :: Verbosity
  , application_id :: String
  , access_token :: String
  , vk_poll_interval_sec :: Int
  , username :: String
  , password :: String
  -- , vk_group_id :: String
  } deriving(Show)

data Pirozhok = Pirozhok
  { plines :: String
  , pdate :: UTCTime
  }

pprint p = liftIO $ do
  -- putStrLn (show $ pdate p)
  putStr (plines p)

type PState a = StateT UTCTime (VKAPI IO) a

instance Error (String,Maybe a) where
  strMsg s = (s,Nothing)

pirozhok d' wr@(WR _ _ _ t _) = Pirozhok <$> poetry <*> date
  where
    poetry = txt >>= nonempty >>= four >>=
             maxlet >>= (pure . unlines)
    date = check (publishedAt wr) where
      check d | d <= d' = oops $ "older than " ++ (show d')
              | otherwise = pure d
    txt = pure $ lines $ gsubRegexPR "<br>" "\n" $ takeWhile (/= '©') t
    nonempty ls = pure $ filter (/=[]) ls
    four ls | length ls >4 = oops "more than 4 lines"
            | otherwise = pure ls
    maxlet ls | (sum $ map length ls) > 250 = oops "more than 250 letters"
              | otherwise = pure ls
    oops s = throwError (s, Just wr)

env_var_name = "VKNEWS_ACCESS_TOKEN"

opts at = Options
  <$> flag Normal Debug (long "verbose" <> help "Be verbose")
  <*> strOption (long "application-id" <> short 'a' <> value vkhs_app_id <> help (printf "Application ID (can be set via %s)" env_var_name))
  <*> strOption (long "access-token" <> short 't' <> value at <> help "Access token")
  <*> option auto (long "poll-interval" <> short 'i' <> value 20 <> help "Poll interval [sec]")
  <*> argument str (metavar "USERNAME" <> help "User name")
  <*> strOption (metavar "STR" <> long "password" <> short 'p' <> value "-" <> help "Password")
  -- <*> argument str (metavar "GROUPID" <> help "Vkontakte ID of the group to read the news from")
  where
    vkhs_app_id = "3128877"

pirozhki = do
  Response (SL len ws) <- lift $ VK.api' "wall.get" [("owner_id",gid_piro)]
  d <- get
  e <- ask
  let ps = map (pirozhok d) ws
  let d' = maxtime d (map pdate $ rights ps)
  forM (lefts ps) $ \(s,wr) -> do
    when (verbose e >= Trace) $ do
      liftIO $ hPutStrLn stderr $ printf "Rejecting record %s. Reason: %s" (maybe "?" (show . wid) wr) s
  when (d' > d) $ do
    when (verbose e >= Trace) $ do
      liftIO $ hPutStrLn stderr $ printf "Updating time to %s" (show d')
    (put d')
  return (rights ps)
  where
    maxtime d [] = d
    maxtime d ps = maximum ps
    gid_piro = "-28122932"

cmd :: Options -> IO ()
cmd (Options v apid at pollint u pass) = run $ do
  forever $ do
    ps <- pirozhki
    forM ps $ \p -> do
        pprint p
        pmsg []
    sleep_sec pollint

    where

      run vk = do
        t <- getCurrentTime
        let e = (VK.env apid u pass VK.allAccess) { verbose = v }
        let ma = runStateT vk t
        r <- runVKAPI ma ([],[],[]) e
        case r of
          Left er -> do
            perror (show er)
            exitFailure
          Right ((a,_),_) -> return a

      perror s = liftIO $ hPutStrLn stderr s

      pmsg s = liftIO $ putStrLn s

      sleep_sec s = liftIO $ threadDelay (1000 * 1000 * s)


main :: IO ()
main = withlib CURL730 $ do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  at <- fromMaybe [] <$> lookupEnv env_var_name
  execParser (info (opts at) idm) >>= cmd

