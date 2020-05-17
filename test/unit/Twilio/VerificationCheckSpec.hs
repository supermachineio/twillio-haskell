{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE InstanceSigs #-}

module Twilio.VerificationCheckSpec where

import Test.Hspec
import Data.Aeson
import Data.Text (Text)

import Control.Exception (AssertionFailed(..), throwIO)
import Data.Aeson as AE
import Twilio.VerificationCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PostVerificationResponseCheck" $ do
    describe "decoding from JSON" $ do
      it "should work" $ do
        result <- AE.eitherDecodeFileStrict "./test/fixtures/verification-check-post-response.json"
        case result of
            (Left err) ->
                throwIO . AssertionFailed $ show err

            (Right resp) -> do
                sid resp `shouldBe` "VEd88fa5b6cb9a5f6a2f1044cecf37ce4a"
                status resp `shouldBe` Approved

  describe "VerificationCheckStatus" $ do
    describe "Aeson instances" $ do
      it "has isomorphic To/FromJSON instances" $ do
        decode (encode Pending)
            `shouldBe` Just Pending

        decode (encode Approved)
            `shouldBe` Just Approved

        decode (encode Denied)
            `shouldBe` Just Denied
