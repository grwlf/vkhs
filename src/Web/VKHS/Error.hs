module Web.VKHS.Error where

import Web.VKHS.Types
import Web.VKHS.Client (Response, Request, URL)
import qualified Web.VKHS.Client as Client

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

data Result t a =
    Fine a
  | UnexpectedInt Error (Int -> t (Result t a) a)
  | UnexpectedBool Error (Bool -> t (Result t a) a)
  | UnexpectedURL Client.Error (URL -> t (Result t a) a)
  | UnexpectedRequest Client.Error (Request -> t (Result t a) a)
  | UnexpectedResponse Client.Error (Response -> t (Result t a) a)

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

