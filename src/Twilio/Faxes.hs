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
  , Twilio.Faxes.post
  ) where

import Control.Monad.Catch
import Data.Text (Text)
import Data.Text.Encoding

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}


data PostFax = PostFax
  { sendTo   :: !Text
  , sendFrom :: !Text
  , sendMediaUrl :: !Text
  } deriving (Show, Eq)

instance Post1 PostFax () where
  post1 msg = request parseJSONFromResponse =<<
    makeTwilioFaxRequest requiredParams
    where requiredParams = [ ("To",   encodeUtf8 $ sendTo msg)
                           , ("From", encodeUtf8 $ sendFrom msg)
                           , ("MediaUrl", encodeUtf8 $ sendMediaUrl msg)
                           ]

-- | Send a text message.
post :: MonadThrow m => PostFax -> TwilioT m ()
post = Resource.post
