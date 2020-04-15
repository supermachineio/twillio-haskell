{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Messages
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Faxes
  ( -- * Resource
    PostFax(..)
  , PostFaxResponse(..)
  , Twilio.Faxes.post
  ) where

import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}


data PostFax = PostFax
  { sendTo   :: !Text
  , sendFrom :: !Text
  , sendMediaUrl :: !Text
  } deriving (Show, Eq)

data PostFaxResponse = PostFaxResponse
  { sid         :: !FaxSID
  , accountSID  :: !AccountSID
  , status      :: !Text
  , to          :: !Text
  , from        :: !Text
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  } deriving (Show)

instance FromJSON PostFaxResponse where
  parseJSON (Object v) = PostFaxResponse
    <$>  v .: "sid"
    <*>  v .: "account_sid"
    <*>  v .: "status"
    <*>  v .: "to"
    <*>  v .: "from"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)

  parseJSON _ = mzero

instance Post1 PostFax PostFaxResponse where
  post1 msg = request parseJSONFromResponse =<<
    makeTwilioFaxRequest requiredParams
    where requiredParams = [ ("To",   encodeUtf8 $ sendTo msg)
                           , ("From", encodeUtf8 $ sendFrom msg)
                           , ("MediaUrl", encodeUtf8 $ sendMediaUrl msg)
                           ]

-- | Send a text message.
post :: MonadThrow m => PostFax -> TwilioT m PostFaxResponse
post = Resource.post
