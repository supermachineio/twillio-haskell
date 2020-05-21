{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}

module Twilio.Verification where

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
import Twilio.VerificationCheck (VerificationCheckStatus)

data Channel = ChannelSMS
  deriving (Show, Eq)


instance FromJSON Channel where
  parseJSON (String v) =
    case v of
      "sms" ->
        pure ChannelSMS

      _ ->
        fail "Unrecognized channel type"

  parseJSON wat =
    AE.typeMismatch "Expected string, but got " wat

channelToParam :: Channel -> Text
channelToParam ch =
  case ch of
    ChannelSMS -> "sms"

data PostVerification = PostVerification
  { verificationTo      :: !Text
  , verificationChannel :: !Channel
  } deriving (Show, Eq)


data PostVerificationResponse = PostVerificationResponse
  { sid         :: !VerificationSID
  , accountSID  :: !AccountSID
  , serviceSID  :: !VerificationServiceSID
  , to          :: !Text
  , channel     :: !Channel
  , status      :: !VerificationCheckStatus
  , valid       :: !Bool
  , url         :: !Text
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  } deriving (Show)


instance FromJSON PostVerificationResponse where
  parseJSON (Object v) = PostVerificationResponse
    <$>  v .: "sid"
    <*>  v .: "account_sid"
    <*>  v .: "service_sid"
    <*>  v .: "to"
    <*>  v .: "channel"
    <*>  v .: "status"
    <*>  v .: "valid"
    <*>  v .: "url"
    <*> (v .: "date_created" >>= parseDateTime')
    <*> (v .: "date_updated" >>= parseDateTime')

  parseJSON wat =
    AE.typeMismatch "Expected object, but got " wat


instance Post2 Text PostVerification PostVerificationResponse where
  post2 verifyServiceId msg =
    let requiredParams = [ ("To",   encodeUtf8 $ verificationTo msg)
                         , ("Channel", encodeUtf8 . channelToParam $ verificationChannel msg)
                         ]
    in
    request parseJSONFromResponse =<<
      makeTwilioVerifyRequest verifyServiceId requiredParams

-- | Send a verification request.
post
    :: MonadThrow m
    => Text
    -> PostVerification
    -> TwilioT m PostVerificationResponse
post serviceID verification = Resource.post serviceID verification
