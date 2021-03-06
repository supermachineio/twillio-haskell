{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Capability
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Types.Capability where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

type Capabilities = Set Capability

data Capability
  = Voice
  | SMS
  | MMS
  | Fax
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

capabilityToJSONString :: Capability -> String
capabilityToJSONString Voice = "voice"
capabilityToJSONString SMS = "SMS"
capabilityToJSONString MMS = "MMS"
capabilityToJSONString Fax = "fax"

toQueryParam :: Capability -> (C.ByteString, Maybe C.ByteString)
toQueryParam Voice = ("VoiceEnabled", Just "true")
toQueryParam SMS = ("SmsEnabled", Just "true")
toQueryParam MMS = ("MmsEnabled", Just "true")
toQueryParam Fax = ("FaxEnabled", Just "true")

instance {-# OVERLAPPING #-} FromJSON Capabilities where
  parseJSON (Object map)
    = let map' = fmap (\value -> case value of
                        Bool bool     -> bool
                        _             -> False) map
      in  return $ foldr (\capability set ->
            if HashMap.lookupDefault False (T.pack $ capabilityToJSONString capability) map'
              then Set.insert capability set
              else set
          ) Set.empty [Voice, SMS, MMS, Fax]
  parseJSON _ = mzero
