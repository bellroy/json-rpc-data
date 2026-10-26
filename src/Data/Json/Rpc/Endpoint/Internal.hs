{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Json.Rpc.Endpoint.Internal
  ( -- * Utilities
    parseRequests,
    partitionByMethod,

    -- * Error Builders
    methodNotFound,
    invalidParams,
    invalidRequest,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IntMap
import Data.Json.Rpc.Types
  ( Error (..),
    ErrorCode (..),
    Request (..),
    RequestId (..),
    Response (..),
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Prelude hiding (id)

parseRequests ::
  IntMap Aeson.Value ->
  (IntMap (Response Aeson.Value Aeson.Value), IntMap (Request Aeson.Value))
parseRequests =
  IntMap.mapEither $
    first invalidRequest . Aeson.parseEither Aeson.parseJSON

partitionByMethod :: IntMap (Request a) -> Map Text (IntMap (Request a))
partitionByMethod = IntMap.foldrWithKey go Map.empty
  where
    go ::
      Key ->
      Request a ->
      Map Text (IntMap (Request a)) ->
      Map Text (IntMap (Request a))
    go i r@Request {method} =
      Map.insertWith IntMap.union method $ IntMap.singleton i r

methodNotFound :: Text -> Response Aeson.Value Aeson.Value
methodNotFound = jsonRpcError MethodNotFound . ("Method not found: " <>)

invalidParams :: String -> Response Aeson.Value Aeson.Value
invalidParams = jsonRpcError InvalidParams . Text.pack

invalidRequest :: String -> Response Aeson.Value Aeson.Value
invalidRequest = jsonRpcError InvalidRequest . Text.pack

jsonRpcError :: ErrorCode -> Text -> Response Aeson.Value Aeson.Value
jsonRpcError code message =
  Response
    { -- We failed to parse but must report an error, spec says to
      -- report @null@ for @"id"@.
      id = RequestIdNull,
      outcome =
        Left $
          Aeson.toJSON @Void
            <$> Error
              { code,
                message,
                data_ = Nothing
              }
    }
