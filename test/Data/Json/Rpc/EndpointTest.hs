{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Data.Json.Rpc.EndpointTest where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Json.Rpc.Endpoint
import qualified Data.Map as Map
import Data.Void (Void)
import Test.Tasty
import Test.Tasty.Golden.Extra.GoldenVsToJSON (goldenVsToJson)

test_rpcCallPositional :: [TestTree]
test_rpcCallPositional =
  [ goldenVsToJson "id 1" "test/golden/rpcCallPositional.id1.golden.json" $ do
      Just input <-
        pure . Aeson.decode $
          "{ \"jsonrpc\": \"2.0\"\
          \, \"method\": \"subtract\"\
          \, \"params\": [42, 23]\
          \, \"id\": 1\
          \}"
      Just res <- subtractPositionalEndpoint input
      pure res,
    goldenVsToJson "id 2" "test/golden/rpcCallPositional.id2.golden.json" $ do
      Just input <-
        pure . Aeson.decode $
          "{ \"jsonrpc\": \"2.0\"\
          \, \"method\": \"subtract\"\
          \, \"params\": [23, 42]\
          \, \"id\": 2\
          \}"
      Just res <- subtractPositionalEndpoint input
      pure res
  ]

test_rpcCallNamed :: [TestTree]
test_rpcCallNamed =
  [ goldenVsToJson "id 3" "test/golden/rpcCallNamed.id3.golden.json" $ do
      Just input <-
        pure . Aeson.decode $
          "{ \"jsonrpc\": \"2.0\"\
          \, \"method\": \"subtract\"\
          \, \"params\": {\"subtrahend\": 23, \"minuend\": 42}\
          \, \"id\": 3\
          \}"
      Just res <- subtractEndpoint input
      pure res,
    goldenVsToJson "id 4" "test/golden/rpcCallNamed.id4.golden.json" $ do
      Just input <-
        pure . Aeson.decode $
          "{ \"jsonrpc\": \"2.0\"\
          \, \"method\": \"subtract\"\
          \, \"params\": {\"subtrahend\": 23, \"minuend\": 42}\
          \, \"id\": 4\
          \}"
      Just res <- subtractEndpoint input
      pure res
  ]

subtractPositionalEndpoint ::
  (Applicative f) => Aeson.Value -> f (Maybe Aeson.Value)
subtractPositionalEndpoint =
  makeSerialEndpoint $ Map.singleton "subtract" subtractHandler
  where
    subtractHandler =
      MethodHandler $ traverseBatch @_ @Void @Int $ \_ (minuend, subtrahend) ->
        pure . Right $ minuend - subtrahend

subtractEndpoint :: (Applicative f) => Aeson.Value -> f (Maybe Aeson.Value)
subtractEndpoint = makeSerialEndpoint $ Map.singleton "subtract" subtractHandler
  where
    subtractHandler =
      MethodHandler $ traverseBatch @_ @Void $ \_ Subtract {..} ->
        pure . Right $ minuend - subtrahend

data Subtract = Subtract
  { minuend :: Int,
    subtrahend :: Int
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Subtract where
  parseJSON = Aeson.withObject "Subtract" $ \o -> do
    minuend <- o .: "minuend"
    subtrahend <- o .: "subtrahend"
    pure Subtract {..}
