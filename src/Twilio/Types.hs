{-#LANGUAGE CPP #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Types
  ( APIVersion(..)
  , module X
    -- * Misc
  , makeTwilioRequest
  , makeTwilioRequest'
  , makeTwilioPOSTRequest
  , makeTwilioPOSTRequest'
  , makeTwilioDELETERequest
  , makeTwilioDELETERequest'
  , makeTwilioFaxRequest
  , makeTwilioVerifyRequest
  , makeTwilioVerificationCheckRequest
  ) where

#if MIN_VERSION_http_client(0,5,0)
#else
import Control.Exception
#endif
import Control.Monad
import Control.Monad.Reader.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_http_client(0,5,0)
import qualified Data.ByteString.Lazy as L
#endif
import Network.HTTP.Client
#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client.Internal (throwHttp)
#endif
import Network.HTTP.Types

import Control.Monad.Twilio
import Twilio.Types.AddressRequirement as X
import Twilio.Types.AuthToken as X
import Twilio.Types.Capability as X
import Twilio.Types.Issue as X
import Twilio.Types.ISOCountryCode as X
import Twilio.Types.List as X
import Twilio.Types.PriceUnit as X
import Twilio.Types.SIDs as X
import Twilio.Internal.Parser
import Twilio.Internal.Request

data APIVersion
  = API_2010_04_01
  | API_2008_08_01
  | Fax_API_V1
  deriving Eq

instance Read APIVersion where
  readsPrec _ = \case
    "2010-04-01" -> return (API_2010_04_01, "")
    "2008-08-01" -> return (API_2008_08_01, "")
    "v1"         -> return (Fax_API_V1, "")
    _            -> mzero

instance Show APIVersion where
  show API_2010_04_01 = "2010-04-01"
  show API_2008_08_01 = "2008-08-01"
  show Fax_API_V1 = "v1"

instance ToJSON APIVersion where
  toJSON apiVersion =
       String . T.pack $ show apiVersion

instance FromJSON APIVersion where
  parseJSON (String "2010-04-01") = return API_2010_04_01
  parseJSON (String "2008-08-01") = return API_2008_08_01
  parseJSON (String "v1")         = return Fax_API_V1
  parseJSON _ = mzero

makeTwilioRequest'' :: Monad m => Text -> Text -> TwilioT m Request
makeTwilioRequest'' myBaseURL suffix = do
  ((accountSID, authToken), _) <- ask
#if MIN_VERSION_http_client(0,4,30)
  let Just request = parseUrlThrow . T.unpack $ myBaseURL <> suffix
#else
  let Just request = parseUrl . T.unpack $ myBaseURL <> suffix
#endif
  return $ applyBasicAuth (C.pack . T.unpack $ getSID accountSID)
                          (C.pack . T.unpack $ getAuthToken authToken) request

makeTwilioRequest' :: Monad m => Text -> TwilioT m Request
makeTwilioRequest' suffix = do
    makeTwilioRequest'' baseURL suffix

makeTwilioRequest :: Monad m => Text -> TwilioT m Request
makeTwilioRequest suffix = do
  ((_, _), accountSID) <- ask
  makeTwilioRequest' $ "/Accounts/" <> getSID accountSID <> suffix

makeTwilioFaxRequest :: Monad m
                       => [(C.ByteString, C.ByteString)]
                       -> TwilioT m Request
makeTwilioFaxRequest params =
  makeTwilioRequest'' baseFaxURL "/Faxes" <&> urlEncodedBody params


makeTwilioVerifyRequest :: Monad m
                       => Text
                       -> [(C.ByteString, C.ByteString)]
                       -> TwilioT m Request
makeTwilioVerifyRequest verifyServiceId params =
  makeTwilioRequest'' verifyBaseURL ("/" <> verifyServiceId <> "/Verifications") <&> urlEncodedBody params


makeTwilioVerificationCheckRequest :: Monad m
                       => Text
                       -> [(C.ByteString, C.ByteString)]
                       -> TwilioT m Request
makeTwilioVerificationCheckRequest verifyServiceId params =
  makeTwilioRequest'' verifyBaseURL ("/" <> verifyServiceId <> "/VerificationCheck") <&> urlEncodedBody params


makeTwilioPOSTRequest' :: Monad m
                       => Text
                       -> [(C.ByteString, C.ByteString)]
                       -> TwilioT m Request
makeTwilioPOSTRequest' resourceURL params =
  makeTwilioRequest' resourceURL <&> urlEncodedBody params


makeTwilioPOSTRequest :: Monad m
                      => Text
                      -> [(C.ByteString, C.ByteString)]
                      -> TwilioT m Request
makeTwilioPOSTRequest resourceURL params =
  makeTwilioRequest resourceURL <&> urlEncodedBody params

makeTwilioDELETERequest' :: Monad m
                         => Text
                         -> TwilioT m Request
makeTwilioDELETERequest' resourceURL =
  makeTwilioRequest' resourceURL <&> (\req -> req {
    method = "DELETE",
#if MIN_VERSION_http_client(0,5,0)
    checkResponse = throwForNon204
#else
    checkStatus = throwForNon204
#endif
  })

makeTwilioDELETERequest :: Monad m
                        => Text
                        -> TwilioT m Request
makeTwilioDELETERequest resourceURL =
  makeTwilioRequest resourceURL <&> (\req -> req {
    method = "DELETE",
#if MIN_VERSION_http_client(0,5,0)
    checkResponse = throwForNon204
#else
    checkStatus = throwForNon204
#endif
  })


#if MIN_VERSION_http_client(0,5,0)
throwForNon204 :: Request -> Response BodyReader -> IO ()
throwForNon204 _ resp =
  case responseStatus resp of
    Status 204 _ -> pure ()
    _ -> do
      chunk <- brReadSome (responseBody resp) 1024
      throwHttp $ StatusCodeException (fmap (const ()) resp) (L.toStrict chunk)
#else
throwForNon204 :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
throwForNon204 (Status 204 _) _ _ = Nothing
throwForNon204 status responseHeaders cookieJar = Just . SomeException
  $ StatusCodeException status responseHeaders cookieJar
#endif
