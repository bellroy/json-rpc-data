{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Json.Rpc.Types
  ( -- * Requests
    Request (..),
    RequestId (..),

    -- * Responses
    Response (..),
    Error (..),
    ErrorCode
      ( ParseError,
        InvalidRequest,
        MethodNotFound,
        InvalidParams,
        InternalError,
        ..
      ),
  )
where

import Data.Aeson ((.:), (.:?), (.=), (.?=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (asum)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)
import Prelude hiding (id)

-- | A JSON-RPC 2.0 Request object.
data Request params = Request
  { -- | If the request ID is absent, it is considered a
    -- "notification" and handlers should not generate a response.
    id :: Maybe RequestId,
    method :: Text,
    params :: params
  }
  deriving stock (Eq, Show, Generic, Foldable, Functor, Traversable)

-- | The three types of request IDs permitted by the JSON-RPC spec.
data RequestId
  = RequestIdString Text
  | RequestIdNumber Scientific
  | RequestIdNull
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON RequestId where
  parseJSON = \case
    Aeson.String t -> pure $ RequestIdString t
    Aeson.Number n -> pure $ RequestIdNumber n
    Aeson.Null -> pure RequestIdNull
    Aeson.Object _ -> fail "Expected string, number, or null; got object"
    Aeson.Array _ -> fail "Expected string, number, or null; got array"
    Aeson.Bool _ -> fail "Expected string, number, or null; got boolean"

instance Aeson.ToJSON RequestId where
  toJSON = \case
    RequestIdString t -> Aeson.toJSON t
    RequestIdNumber n -> Aeson.toJSON n
    RequestIdNull -> Aeson.toJSON Aeson.Null

  toEncoding = \case
    RequestIdString t -> Aeson.toEncoding t
    RequestIdNumber n -> Aeson.toEncoding n
    RequestIdNull -> Aeson.toEncoding Aeson.Null

-- | For this instance to be lawful, @params@ must parse from either a
-- JSON Array or Object.
instance
  (Aeson.FromJSON params, Typeable params) =>
  Aeson.FromJSON (Request params)
  where
  parseJSON =
    Aeson.withObject objectName $ \o -> do
      -- TODO: check for "jsonrpc": "2.0"
      id <- o .:? "id"
      method <- o .: "method"
      params <- o .: "params"
      pure Request {..}
    where
      objectName = "Request(" ++ show (typeRep $ Proxy @params) ++ ")"

-- | For this instance to be lawful, @params@ must encode to either a
-- JSON Array or Object.
instance (Aeson.ToJSON params) => Aeson.ToJSON (Request params) where
  toJSON Request {..} =
    Aeson.Object $
      mconcat
        [ "jsonrpc" .= Aeson.String "2.0",
          "id" .?= id,
          "method" .= method,
          "params" .= params
        ]

  toEncoding Request {..} =
    Aeson.pairs $
      mconcat
        [ "jsonrpc" .= Aeson.String "2.0",
          "id" .?= id,
          "method" .= method,
          "params" .= params
        ]

-- | A JSON-RPC 2.0 response object. It represents either a response
-- to a non-notification request, or an error.
data Response errorData resultData = Response
  { id :: RequestId,
    outcome :: Either (Error errorData) resultData
  }
  deriving stock (Eq, Show, Generic, Foldable, Functor, Traversable)

instance Bifunctor Response where
  bimap f g Response {..} = Response {outcome = bimap (fmap f) g outcome, ..}

instance
  ( Aeson.FromJSON errorData,
    Typeable errorData,
    Aeson.FromJSON resultData,
    Typeable resultData
  ) =>
  Aeson.FromJSON (Response errorData resultData)
  where
  parseJSON =
    Aeson.withObject objectName $ \o -> do
      -- TODO: check for "jsonrpc": "2.0"
      id <- o .: "id"
      outcome <-
        asum
          [ Left <$> (o .: "error" >>= Aeson.parseJSON),
            Right <$> (o .: "result" >>= Aeson.parseJSON)
          ]
      pure Response {..}
    where
      objectName = "Response(" ++ show (typeRep $ Proxy @resultData) ++ ")"

instance
  (Aeson.ToJSON errorData, Aeson.ToJSON resultData) =>
  Aeson.ToJSON (Response errorData resultData)
  where
  toJSON Response {..} =
    Aeson.Object $
      mconcat
        [ "jsonrpc" .= Aeson.String "2.0",
          "id" .= id,
          case outcome of
            Left err -> "error" .= Aeson.toJSON err
            Right res -> "result" .= Aeson.toJSON res
        ]

  toEncoding Response {..} =
    Aeson.pairs $
      mconcat
        [ "jsonrpc" .= Aeson.String "2.0",
          "id" .= id,
          case outcome of
            Left err -> "error" .= Aeson.toJSON err
            Right res -> "result" .= Aeson.toJSON res
        ]

-- | The error object within a JSON-RPC 2.0 response object.
data Error data_ = Error
  { code :: ErrorCode,
    message :: Text,
    data_ :: Maybe data_
  }
  deriving stock (Eq, Show, Generic, Foldable, Functor, Traversable)

-- | Error codes returned by JSON-RPC 2.0 errors.
--
-- This newtype bundles pattern synonyms for errors enumerated in the
-- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification).
-- You may wish to define additional ones for the errors returned by
-- your methods.
newtype ErrorCode = ErrorCode Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Num)

pattern ParseError :: ErrorCode
pattern ParseError = -32700

pattern InvalidRequest :: ErrorCode
pattern InvalidRequest = -32600

pattern MethodNotFound :: ErrorCode
pattern MethodNotFound = -32601

pattern InvalidParams :: ErrorCode
pattern InvalidParams = -32602

pattern InternalError :: ErrorCode
pattern InternalError = -32603

instance
  (Aeson.FromJSON data_, Typeable data_) =>
  Aeson.FromJSON (Error data_)
  where
  parseJSON = Aeson.withObject objectName $ \o -> do
    code <- o .: "code"
    message <- o .: "message"
    data_ <- o .:? "data"
    pure Error {..}
    where
      objectName = "Error(" ++ show (typeRep $ Proxy @data_) ++ ")"

instance (Aeson.ToJSON data_) => Aeson.ToJSON (Error data_) where
  toJSON Error {..} =
    Aeson.Object $
      mconcat
        [ "code" .= code,
          "message" .= message,
          "data" .?= data_
        ]

  toEncoding Error {..} =
    Aeson.pairs $
      mconcat
        [ "code" .= code,
          "message" .= message,
          "data" .?= data_
        ]
