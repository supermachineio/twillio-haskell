{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE InstanceSigs #-}

module Twilio.FaxesSpec where

import Test.Hspec
import Data.Aeson
import Data.Text (Text)

import Control.Exception (AssertionFailed(..), throwIO)
import Data.Aeson as AE
import Twilio.Faxes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PostFaxResponse" $ do
    describe "decoding from JSON" $ do
      it "should work" $ do
        result <- AE.eitherDecodeFileStrict "./test/fixtures/fax-post-response.json"
        case result of
            (Left err) ->
                throwIO . AssertionFailed $ show err

            (Right resp) ->
                faxResponseSid resp `shouldBe` "FXd87be9ff8c935704925b0daa60c1e0dd"
