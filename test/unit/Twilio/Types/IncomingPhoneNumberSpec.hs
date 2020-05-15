{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE InstanceSigs #-}
module Twilio.Types.IncomingPhoneNumberSpec where

import Test.Hspec
import Twilio.Types.PriceUnit
import Data.Aeson
import Data.Set as Set
import Test.QuickCheck hiding (Success)
import Data.Text (Text)
import Test.QuickCheck.Instances ()

import Control.Exception (AssertionFailed(..), throwIO)
import Data.Aeson as AE
import Twilio.IncomingPhoneNumber
import Twilio.Types.Capability

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AvailablePhoneNumber" $ do
    describe "decoding from JSON" $ do
      it "should work" $ do
        result <- AE.eitherDecodeFileStrict "./test/fixtures/incoming-phone-number.json"
        case result of
            (Left err) ->
                throwIO . AssertionFailed $ show err

            (Right resp) -> do
                friendlyName resp `shouldBe` "(567) 275-8578"
                capabilities resp `shouldBe` Set.fromList [Voice, Fax]
