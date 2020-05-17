{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}

module Twilio.VerificationCheck where

import Control.Monad.Catch
import Data.Aeson
import qualified Data.Aeson.Types as AE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

data PostVerificationCheck = PostVerificationCheck
  { verificationCheckTo   :: !Text
  , verificationCheckCode :: !Text
  } deriving (Show, Eq)


data VerificationCheckStatus
    = Pending
    | Approved
    | Denied
    deriving (Show, Eq)


instance ToJSON VerificationCheckStatus where
  toJSON status =
    case status of
        Pending -> AE.String "pending"
        Approved -> AE.String "approved"
        Denied -> AE.String "denied"


instance FromJSON VerificationCheckStatus where
  parseJSON (String v) =
    case v of
      "pending" -> pure Pending
      "approved" -> pure Approved
      "denied" -> pure Denied
      wat -> fail $ "Unrecognized status: " <> T.unpack wat

  parseJSON wat =
    AE.typeMismatch "Expected string, but got " wat


data PostVerificationCheckResponse = PostVerificationCheckResponse
  { sid         :: !VerificationSID
  , accountSID  :: !AccountSID
  , serviceSID  :: !VerificationServiceSID
  , to          :: !Text
  , status      :: !VerificationCheckStatus
  , valid       :: !Bool
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  } deriving (Show)


instance FromJSON PostVerificationCheckResponse where
  parseJSON (Object v) = PostVerificationCheckResponse
    <$>  v .: "sid"
    <*>  v .: "account_sid"
    <*>  v .: "service_sid"
    <*>  v .: "to"
    <*>  v .: "status"
    <*>  v .: "valid"
    <*> (v .: "date_created" >>= parseDateTime')
    <*> (v .: "date_updated" >>= parseDateTime')

  parseJSON wat =
    AE.typeMismatch "Expected object, but got " wat


instance Post2 Text PostVerificationCheck PostVerificationCheckResponse where
  post2 verifyServiceId msg =
    let requiredParams = [ ("To",   encodeUtf8 $ verificationCheckTo msg)
                         , ("Code", encodeUtf8 $ verificationCheckCode msg)
                         ]
    in
    request parseJSONFromResponse =<<
      makeTwilioVerificationCheckRequest verifyServiceId requiredParams

-- | Send a verification request.
post
    :: MonadThrow m
    => Text
    -> PostVerificationCheck
    -> TwilioT m PostVerificationCheckResponse
post serviceID verificationCheck = Resource.post serviceID verificationCheck
