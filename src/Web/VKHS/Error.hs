module Web.VKHS.Error where

import Web.VKHS.Types
import Web.VKHS.Client (Request, URL)
import qualified Web.VKHS.Client as Client

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

data Result' k m a =
    Fine a
  | MFine (m a) {- MFine tolds the GHC that the real kind of m is (* -> *) -}
  | UnexpectedInt Error (Int -> k (Result' k m a) m a)
  | UnexpectedBool Error (Bool -> k (Result' k m a) m a)
  | UnexpectedURL Client.Error (URL -> k (Result' k m a) m a)
  | UnexpectedRequest Client.Error (Request -> k (Result' k m a) m a)

data ResultDescription a =
    DescFine a
  | DescVKError Error
  | DescClientError Client.Error
  deriving(Show)

describeResult :: Result' k m a -> ResultDescription a
describeResult (Fine a) = DescFine a
describeResult (UnexpectedInt e k) = DescVKError e
describeResult (UnexpectedBool e k) = DescVKError e
describeResult (UnexpectedURL e k) = DescClientError e
describeResult (UnexpectedRequest e k) = DescClientError e

