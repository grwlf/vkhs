module Web.VKHS.Error where

import Web.VKHS.Types
import Web.VKHS.Client (Response, Request, URL)
import qualified Web.VKHS.Client as Client

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

type R t a = Result t a

data Result t a =
    Fine a
  | UnexpectedInt Error (Int -> t (R t a) (R t a))
  | UnexpectedBool Error (Bool -> t (R t a) (R t a))
  | UnexpectedURL Client.Error (URL -> t (R t a) (R t a))
  | UnexpectedRequest Client.Error (Request -> t (R t a) (R t a))
  | UnexpectedResponse Client.Error (Response -> t (R t a) (R t a))
  | UnexpectedFormField Form String (String -> t (R t a) (R t a))

data ResultDescription a =
    DescFine a
  | DescVKError Error
  | DescClientError Client.Error
  deriving(Show)

describeResult :: Result t a -> ResultDescription a
describeResult (Fine a) = DescFine a
describeResult (UnexpectedInt e k) = DescVKError e
describeResult (UnexpectedBool e k) = DescVKError e
describeResult (UnexpectedURL e k) = DescClientError e
describeResult (UnexpectedRequest e k) = DescClientError e

