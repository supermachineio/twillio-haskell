{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}
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
  , FaxCallbackPayload(..)
  , FaxStatus(..)
  , IncomingFaxPayload(..)
  , Twilio.Faxes.post
  ) where

import Control.Monad.Catch
import Data.Aeson
import qualified Data.Aeson.Types as AE
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


data FaxStatus
    = Queued     -- The fax is queued, waiting for processing
    | Processing -- The fax is being downloaded, uploaded, or transcoded into a different format
    | Sending    -- The fax is in the process of being sent
    | Delivered  -- The fax has been successfuly delivered
    | Receiving  -- The fax is in the process of being received
    | Received   -- The fax has been successfully received
    | NoAnswer   -- The outbound fax failed because the other end did not pick up
    | Busy       -- The outbound fax failed because the other side sent back a busy signal
    | Failed     -- The fax failed to send or receive
    | Canceled   -- The fax was canceled, either by using the REST API, or rejected by TwiML
    | Other Text
    deriving (Show)


instance FromJSON FaxStatus where
    parseJSON (String str) =
        case str of
            "queued"     -> pure Queued
            "processing" -> pure Processing
            "sending"    -> pure Sending
            "delivered"  -> pure Delivered
            "receiving"  -> pure Receiving
            "received"   -> pure Received
            "no-answer"  -> pure NoAnswer
            "busy"       -> pure Busy
            "failed"     -> pure Failed
            "canceled"   -> pure Canceled
            wat          -> pure (Other wat)
    parseJSON wat =
        AE.typeMismatch "Expected string, but got " wat

instance ToJSON FaxStatus where
    toJSON Queued      = String "queued"
    toJSON Processing  = String "processing"
    toJSON Sending     = String "sending"
    toJSON Delivered   = String "delivered"
    toJSON Receiving   = String "receiving"
    toJSON Received    = String "received"
    toJSON NoAnswer    = String "no-answer"
    toJSON Busy        = String "busy"
    toJSON Failed      = String "failed"
    toJSON Canceled    = String "canceled"
    toJSON (Other str) = String str


data IncomingFaxPayload = IncomingFaxPayload
    { sid        :: !FaxSID
    , apiVersion :: !APIVersion
    , to         :: !Text
    , from       :: !Text
    , accountSid :: !AccountSID
    } deriving (Show)


data FaxCallbackPayload = FaxCallbackPayload
    { faxSid     :: !FaxSID
    , apiVersion :: !APIVersion
    , mediaUrl   :: !Text
    , numPages   :: !Int
    , faxStatus  :: !FaxStatus
    , to         :: !Text
    , from       :: !Text
    , accountSID :: !AccountSID
    } deriving (Show)

instance Eq FaxCallbackPayload where
    (==) p1 p2 =
        faxSid p1 == faxSid p2

instance FromJSON FaxCallbackPayload where
  parseJSON (Object v) = FaxCallbackPayload
    <$>  v .: "FaxSid"
    <*>  v .: "ApiVersion"
    <*>  v .: "MediaUrl"
    <*>  v .: "NumPages"
    <*>  v .: "FaxStatus"
    <*>  v .: "To"
    <*>  v .: "From"
    <*>  v .: "AccountSid"

  parseJSON wat =
    AE.typeMismatch "Expected object, but got " wat

instance ToJSON FaxCallbackPayload where
    toJSON FaxCallbackPayload{..} =
        object
            [ "FaxSid" .= faxSid
            , "ApiVersion" .= apiVersion
            , "MediaUrl" .= mediaUrl
            , "NumPages" .= numPages
            , "FaxStatus" .= faxStatus
            , "To" .= to
            , "From" .= from
            , "AccountSid" .= accountSID
            ]

data PostFaxResponse = PostFaxResponse
  { faxResponseSid         :: !FaxSID
  , faxResponseAccountSID  :: !AccountSID
  , faxResponseStatus      :: !Text
  , faxResponseTo          :: !Text
  , faxResponseFrom        :: !Text
  , faxResponseDateCreated :: !UTCTime
  , faxResponseDateUpdated :: !UTCTime
  } deriving (Show)

instance FromJSON PostFaxResponse where
  parseJSON (Object v) = PostFaxResponse
    <$>  v .: "sid"
    <*>  v .: "account_sid"
    <*>  v .: "status"
    <*>  v .: "to"
    <*>  v .: "from"
    <*> (v .: "date_created" >>= parseDateTime')
    <*> (v .: "date_updated" >>= parseDateTime')

  parseJSON wat =
    AE.typeMismatch "Expected object, but got " wat


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
