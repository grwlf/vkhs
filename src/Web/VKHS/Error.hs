{-# LANGUAGE RecordWildCards #-}

module Web.VKHS.Error where

import Web.VKHS.Types
import Web.VKHS.Client (Response, Request, URL)
import qualified Web.VKHS.Client as Client
import Data.ByteString.Char8 (ByteString, unpack)

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq)

-- | Alias for Result
type R t a = Result t a

-- | Result with continuation. @t@ represents the continuation monad, which
-- needs to track two types: the 'early break' type and the 'current result'
-- type. In the end both types are the same.
data Result t a =
    Fine a
  -- ^ The normal exit of a computation
  | UnexpectedInt Error (Int -> t (R t a) (R t a))
  -- ^ Invalid integer value. It is possible for client to set a correct URL and
  -- continue
  | UnexpectedBool Error (Bool -> t (R t a) (R t a))
  -- ^ Invalid boolean value. It is possible for client to set a correct URL and
  -- continue
  | UnexpectedURL Client.Error (URL -> t (R t a) (R t a))
  -- ^ Invalid URL. It is possible for client to set a correct URL and continue
  | UnexpectedRequest Client.Error (Request -> t (R t a) (R t a))
  | UnexpectedResponse Client.Error (Response -> t (R t a) (R t a))
  | UnexpectedFormField Form String (String -> t (R t a) (R t a))
  | LoginActionsExhausted
  | RepeatedForm Form (() -> t (R t a) (R t a))
  | JSONParseFailure ByteString (JSON -> t (R t a) (R t a))
  | JSONParseFailure' JSON String

data ResultDescription a =
    DescFine a
  | DescError String
  deriving(Show)

describeResult :: (Show a) => Result t a -> String
describeResult (Fine a) = "Fine " ++ show a
describeResult (UnexpectedInt e k) = "UnexpectedInt " ++ (show e)
describeResult (UnexpectedBool e k) = "UnexpectedBool " ++  (show e)
describeResult (UnexpectedURL e k) = "UnexpectedURL " ++ (show e)
describeResult (UnexpectedRequest e k) = "UnexpectedRequest " ++ (show e)
describeResult LoginActionsExhausted = "LoginActionsExhausted"
describeResult (RepeatedForm f k) = "RepeatedForm"
describeResult (JSONParseFailure bs _) = "JSONParseFailure " ++ (show bs)
describeResult (JSONParseFailure' JSON{..} s) = "JSONParseFailure' " ++ (show s) ++ " JSON: " ++ (take 1000 $ show js_aeson)

