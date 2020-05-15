{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.AvailablePhoneNumbers
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.AvailablePhoneNumbers
  ( -- * Resource
    AvailablePhoneNumbers(..)
  , AvailablePhoneNumbersReq(..)
  , Twilio.AvailablePhoneNumbers.get
  , Twilio.AvailablePhoneNumbers.get1
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as URI

import Control.Monad.Twilio
import Twilio.AvailablePhoneNumber
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types
import qualified Twilio.Types.Capability as Capability

{- Resource -}

data AvailablePhoneNumbersReq = AvailablePhoneNumbersReq
    { reqIsoCountryCode :: ISOCountryCode
    , reqCapabilities :: Set Capability
    }

data AvailablePhoneNumbers = AvailablePhoneNumbers
  { availablePhoneNumberList :: [AvailablePhoneNumber]
  } deriving (Show, Eq)

instance List AvailablePhoneNumbers AvailablePhoneNumber where
  getListWrapper = wrap (const AvailablePhoneNumbers)
  getList = availablePhoneNumberList
  getPlural = Const "available_phone_numbers"

instance FromJSON AvailablePhoneNumbers where
  parseJSON = parseJSONToList

instance Get1 ISOCountryCode AvailablePhoneNumbers where
  get1 isoCountryCode = request parseJSONFromResponse =<< makeTwilioRequest
    ("/AvailablePhoneNumbers/" <> T.pack (show isoCountryCode) <> "/Local.json")

instance Get1 AvailablePhoneNumbersReq AvailablePhoneNumbers where
  get1 req = request parseJSONFromResponse =<< makeTwilioRequest
    (url <> params)
    where
        url = "/AvailablePhoneNumbers/" <> T.pack (show (reqIsoCountryCode req)) <> "/Local.json"
        params = TE.decodeUtf8 . URI.renderQuery True $ fmap Capability.toQueryParam (Set.toList $ reqCapabilities req)

-- | Get 'AvailablePhoneNumbers' for a particular country.
get :: MonadThrow m => ISOCountryCode -> TwilioT m AvailablePhoneNumbers
get = Resource.get

-- | Get 'AvailablePhoneNumbers' for a particular country.
get1 :: MonadThrow m => AvailablePhoneNumbersReq -> TwilioT m AvailablePhoneNumbers
get1 = Resource.get1
