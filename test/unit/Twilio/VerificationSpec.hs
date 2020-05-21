{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE InstanceSigs #-}

module Twilio.VerificationSpec where

import Test.Hspec
import Data.Aeson
import Data.Text (Text)

import Control.Exception (AssertionFailed(..), throwIO)
import Data.Aeson as AE
import Twilio.Verification

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PostVerificationResponse" $ do
    describe "decoding from JSON" $ do
      it "should work" $ do
        result <- AE.eitherDecodeFileStrict "./test/fixtures/verification-post-response.json"
        case result of
            (Left err) ->
                throwIO . AssertionFailed $ show err

            (Right resp) -> do
                url resp `shouldBe` "https://verify.twilio.com/v2/Services/VAd88fa5b6cb9a5f6a2f1044cecf37ce4a/Verifications/VEd88fa5b6cb9a5f6a2f1044cecf37ce4a"

