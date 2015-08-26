{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.VKHS.Login
    ( login
    , env
    , getUserAccessTokenStep1
    , getUserAccessTokenStep2
    ) where

import Prelude hiding ((.), id, catch)

import Control.Applicative
import Control.Category
import Control.Exception
import Control.Failure
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Monad.State as S

import qualified Data.ByteString as BS
import Data.List
import Data.String
import Data.Char
import Data.Monoid
import Data.Either
import Data.Label
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable

import Network.CURL730
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Protocol.Uri.Query
import Network.Protocol.Cookie as C
import Network.Shpider.Forms

import Network.HTTP.Conduit hiding (Response)
import Data.Aeson as Aes

import Text.HTML.TagSoup
import Text.Printf
import System.IO

import Web.VKHS.Types
import Web.VKHS.Curl as VKHS

-- Test applications:
--
-- pirocheck
-- ID 3115622
-- Key IOmaB10C1v7RoMiM6Lnu
--
-- pirofetch
-- ID 3082266
-- Key <lost>

type Url = String
type Email = String
type Uid = String
type Password = String
type Body = String

toarg :: [AccessRight] -> String
toarg = intercalate "," . map (map toLower . show)

-- | Gathers login information into Env data set.
env :: String
    -- ^ Client ID (provided by VKontakte, also known as application ID)
    -> String
    -- ^ Client secret
    -> String
    -- ^ User email, able to authenticate the user
    -> String
    -- ^ User password
    -> [AccessRight]
    -- ^ Access rights to request
    -> Env LoginEnv
env cid csec email pwd ar = mkEnv
  (LoginEnv
    [ ("email",email) , ("pass",pwd) ]
    ar
    cid
    csec
  )

vk_start_action :: ClientId -> [AccessRight] -> ActionHist
vk_start_action cid ac = AH [] $ OpenUrl start_url mempty where
    start_url = (\f -> f $ toUri "https://oauth.vk.com/authorize")
        $ set query $ bw params
            [ ("client_id",     cid)
            , ("scope",         toarg ac)
            , ("redirect_uri",  "https://oauth.vk.com/blank.html")
            , ("display",       "wap")
            , ("response_type", "token")
            ]

type FilledForm = Form

type VK a = ReaderT (Env LoginEnv) (ErrorT String IO) a

runVK :: Env LoginEnv -> VK a -> IO (Either String a)
runVK e vk = runErrorT (runReaderT vk e)

liftVK :: IO (Either String a) -> VK a
liftVK m = liftIO m >>= either fail return

dbgVK :: Verbosity -> IO () -> VK ()
dbgVK v act = ask >>= \e -> do
        if v > (verbose e) then return ()
                            else liftIO $ act

when_debug = dbgVK Debug
when_trace = dbgVK Trace

type Page = (Http Response, Body)

-- | Browser action

type Hist = [[String]]

data ActionHist = AH Hist Action
  deriving(Show)

data Action = OpenUrl Uri Cookies | SendForm Form Cookies
  deriving (Show)

-- | Url assotiated with Action
actionUri :: Action -> Uri
actionUri (OpenUrl u _) = u
actionUri (SendForm f _) = toUri . action $ f

liftEIO act = (liftIO act) >>= either fail return

-- | Send a get-request to the server
vk_get :: Uri -> Cookies -> VK Page
vk_get u c = let
    c' = (bw cookie $ map toShort $ bw gather c )
    u' = (showUri u)
    in do
        e <- ask
        s <- liftEIO $ vk_curl e $ do
            when ((not . null) c') $ do
                tell [CURLOPT_COOKIE c']
            when ((not . null) u') $ do
                tell [CURLOPT_URL u']
        liftVK (return $ parseResponse $ VKHS.unpack s)

-- | Send a form to the server.
vk_post :: FilledForm -> Cookies -> VK Page
vk_post f c = let
    c' = (bw cookie $ map toShort $ bw gather c )
    p' = (bw params $ M.toList $ inputs f)
    u' = (action f)
    in do
        e <- ask
        s <- liftEIO $ vk_curl e $ do
            when ((not . null) c') $ do
                tell [CURLOPT_COOKIE c']
            when ((not . null) u') $ do
                tell [CURLOPT_URL u']
            tell [CURLOPT_POST True]
            tell [CURLOPT_COPYPOSTFIELDS p']
        liftVK (return $ parseResponse $ VKHS.unpack s)

-- | Splits parameters into 3 categories:
-- 1)without a value, 2)filled from user dictionary, 3)with default values
split_inputs :: [(String,String)]
             -- ^ User dictionary
             -> M.Map String String
             -- ^ Fields with default values (default doesn't exist if zero string)
             -> (M.Map String (), M.Map String String, M.Map String String)
split_inputs d m =
    let (b,g) = M.mapEitherWithKey (match_field d) m
    in (b, M.map (either id id) g, snd (M.mapEither id g))
    where
        match_field d k a
            | not (null a) = maybe ((Right . Left) a)  (Right . Right) u
            | otherwise    = maybe (Left ())           (Right . Right) u
            where u = lookup k d

inputs_to_fill :: Form -> [String]
inputs_to_fill f = let (inps,_,_) = split_inputs [] (inputs f) in M.keys inps

vk_fill_form :: Form -> VK FilledForm
vk_fill_form f = ask >>= \e -> do
    let (bad,good,user) = split_inputs ((formdata . sub) e) (inputs f)
    when (not $ M.null bad) (fail $ "Unmatched form parameters: " ++ (show bad))
    return f { inputs = good }

-- | Execute an action, return Web-server's answer and adjusted cookies. Cookie
-- management is very primitive: it's no more than merging old and new ones
vk_move :: Action -> VK (Page, Cookies)
vk_move (OpenUrl u c) = do
    (h,b) <- (vk_get u c)
    return ((h,b),c`mappend`(get setCookies h))
vk_move (SendForm f c) = do
    f' <- vk_fill_form f
    (h,b) <- (vk_post f' c)
    return ((h,b),c`mappend`(get setCookies h))

uri_fragment :: Http Response -> Maybe AccessToken
uri_fragment = get location >=> pure . get fragment >=> pure . fw (keyValues "&" "=") >=> \f -> do
    (\a b c -> (a,b,c)) <$> lookup "access_token" f <*> lookup "user_id" f <*> lookup "expires_in" f

-- | Suggest new action
vk_analyze :: Hist -> (Page,Cookies) -> VK (Either AccessToken (Action,Hist))
vk_analyze hist ((h,b),c)
    | isJust a        = return $ Left (fromJust a)
    | isJust l        = return $ Right (OpenUrl (fromJust l) c, hist)
    | not (null fs) && not (is `elem` hist)
                      = return $ Right (SendForm f c, is:hist)
    | not (null fs) && (is `elem` hist)
                      = fail $ "Invalid password/Form containig following inputs already seen: " ++ (show is)
    | not gs          = fail $ "HTTP error: status " ++ (show s)
    | otherwise       = fail $ "HTML processing failure (new design of VK login dialog?)"
    where
        gs = s`elem` [OK,Created,Found,SeeOther,Accepted,Continue]
        s = get status h
        l = get location h
        fs = parseTags >>> gatherForms $ b
        f = head fs
        is = inputs_to_fill f
        a = uri_fragment h

vk_dump_page :: Int -> Uri -> Page -> IO ()
vk_dump_page n u (h,b)
  | (>0) . length . filter (isAlpha) $ b =
    let name = printf "%02d-%s.html" n (showAuthority (get authority u))
    in bracket (openFile name WriteMode) (hClose) $ \f -> do
        hPutStrLn f b
        hPutStrLn stderr $ "dumped: name " ++ (name) ++ " size " ++ (show $ length b)
  | otherwise = return ()

-- | Execute login procedure, return (Right AccessToken) on success
login :: Env LoginEnv -> IO (Either String AccessToken)
login e@(Env (LoginEnv _ acr cid _) _ _ _) =
  runVK e $ loop [0..] (vk_start_action cid acr) where
    loop (n:ns) (AH h act) = do
        when_trace $ printf "VK => %02d %s" n (show act)
        ans@(p,c) <- vk_move act
        when_debug $ vk_dump_page n (actionUri act) p
        a <- vk_analyze h ans
        case a of
            Right (act',h') -> do
                loop ns (AH h' act')
            Left at -> do
                return at

type RedirectUrl = String

getUserAccessTokenStep1 :: Env LoginEnv -> RedirectUrl -> IO (String)
getUserAccessTokenStep1 e@(Env (LoginEnv _ perms cid _) _ _ _) redirectUrl = do
  return $ concat $ urlBase
         : cid
         : "&response_type=code"
         : "&display=page"
         : "&redirect_uri="
         : redirectUrl
         : ( case perms of
                [] -> []
                _  -> pure $ "&scope=" ++ (toarg perms)
           )
  where
    urlBase = "https://oauth.vk.com/authorize?client_id="

data AccessTok = AccessTok { access_token :: String
                           , expires_in :: Int
                           , user_id :: Int
                           }
instance FromJSON AccessTok where
    parseJSON (Object v) = AccessTok <$>
                           v .: "access_token" <*>
                           v .: "expires_in" <*>
                           v .: "user_id"
    parseJSON _          = mzero

type Code = String

getUserAccessTokenStep2 :: Env LoginEnv -> RedirectUrl -> Code -> IO (Either String AccessToken)
getUserAccessTokenStep2 e@(Env (LoginEnv _ _ cid csec) _ _ _) redirectUrl code =
  do
    mAt <- simpleHttp requestUrl >>= return . (\v -> (Aes.decode v) :: Maybe AccessTok)
    case mAt of
        Just (AccessTok at ex uid) ->return $ Right (at, show uid, show ex)
        Nothing -> return $ Left "Something went wrong. Check your code."
  where
    urlBase = "https://oauth.vk.com/access_token?client_id="
    requestUrl = concat $ urlBase
                    : cid
                    : "&client_secret="
                    : csec
                    : "&redirect_uri="
                    : redirectUrl
                    : "&code="
                    : [code]
