{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.IncomingPhoneNumbers
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.IncomingPhoneNumbers
  ( -- * Resource
    IncomingPhoneNumbers(..)
  , PostIncomingPhoneNumber(..)
  , VoiceReceiveMode(..)
  , Twilio.IncomingPhoneNumbers.get
  , Twilio.IncomingPhoneNumbers.post
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding

import Control.Monad.Twilio
import Twilio.IncomingPhoneNumber
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data IncomingPhoneNumbers = IncomingPhoneNumbers
  { incomingPhoneNumberList :: [IncomingPhoneNumber]
  } deriving (Show, Eq)

data PostIncomingPhoneNumber = PostIncomingPhoneNumber
    { reqPhoneNumber :: !Text
    , reqVoiceUrl :: !Text
    , reqVoiceReceiveMode :: !VoiceReceiveMode
    , reqStatusCallbackUrl :: !Text
    }

data VoiceReceiveMode
    = FaxMode
    | VoiceMode

voiceReceiveModeToParam :: VoiceReceiveMode -> Text
voiceReceiveModeToParam FaxMode = "fax"
voiceReceiveModeToParam VoiceMode = "voice"

instance List IncomingPhoneNumbers IncomingPhoneNumber where
  getListWrapper = wrap (const IncomingPhoneNumbers)
  getList = incomingPhoneNumberList
  getPlural = Const "incoming_phone_numbers"

instance FromJSON IncomingPhoneNumbers where
  parseJSON = parseJSONToList

instance Get0 IncomingPhoneNumbers where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/IncomingPhoneNumbers.json"

instance Post1 PostIncomingPhoneNumber IncomingPhoneNumber where
  post1 msg = request parseJSONFromResponse =<<
    makeTwilioPOSTRequest "/IncomingPhoneNumbers.json" requiredParams
    where requiredParams = [ ("PhoneNumber", encodeUtf8 $ reqPhoneNumber msg)
                           , ("VoiceReceiveMode", encodeUtf8 . voiceReceiveModeToParam $ reqVoiceReceiveMode msg)
                           , ("VoiceUrl", encodeUtf8 $ reqVoiceUrl msg)
                           , ("StatusCallbackUrl", encodeUtf8 $ reqStatusCallbackUrl msg)
                           ]

-- | Get 'IncomingPhoneNumbers' for a particular country.
get :: MonadThrow m => TwilioT m IncomingPhoneNumbers
get = Resource.get
-- | Send a text message.
post :: MonadThrow m => PostIncomingPhoneNumber -> TwilioT m IncomingPhoneNumber
post = Resource.post
