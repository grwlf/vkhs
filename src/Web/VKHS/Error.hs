{-# LANGUAGE RecordWildCards #-}

module Web.VKHS.Error where

import Web.VKHS.Types
import Web.VKHS.Client (Response, Request, URL)
import qualified Web.VKHS.Client as Client
import Data.ByteString.Char8 (ByteString, unpack)

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq)

type R t a = Result t a

data Result t a =
    Fine a
  | UnexpectedInt Error (Int -> t (R t a) (R t a))
  | UnexpectedBool Error (Bool -> t (R t a) (R t a))
  | UnexpectedURL Client.Error (URL -> t (R t a) (R t a))
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
describeResult (JSONParseFailure' JSON{..} s) = "JSONParseFailure' " ++ (show s) ++ " JSON: " ++ (show js_aeson)

