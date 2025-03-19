{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Json.Rpc.Endpoint where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Functor ((<&>))
import Data.Json.Rpc.Types (Error (..), ErrorCode (..), Request (..), RequestId (..), Response (..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Internals.Async (Flat (..), FlatApp (..), runFlat)
import Witherable
import Prelude hiding (id)

data MethodHandler f where
  MethodHandler ::
    (Witherable t) =>
    ( t (Request Aeson.Value) ->
      f (t (Response Aeson.Value Aeson.Value))
    ) ->
    MethodHandler f

makeEndpoint ::
  (Monad m) =>
  Map Text (MethodHandler m) ->
  Aeson.Value ->
  Maybe Aeson.Value
makeEndpoint = undefined

-- | traverseBatchConcurrently is to traverseBatch as mapConcurrently is to traverse
traverseBatchConcurrently ::
  forall params errorData resultData t m.
  ( Witherable t,
    MonadUnliftIO m,
    Aeson.FromJSON params,
    Aeson.ToJSON errorData,
    Aeson.ToJSON resultData
  ) =>
  (Maybe RequestId -> params -> m (Either (Error errorData) resultData)) ->
  t (Request Aeson.Value) ->
  m (t (Response Aeson.Value Aeson.Value))
traverseBatchConcurrently f t = withRunInIO $ \run ->
  runFlat $
    traverseBatch
      (\mReq params -> FlatApp . FlatAction . run $ f mReq params)
      t

traverseBatch ::
  forall params errorData resultData t f.
  ( Witherable t,
    Applicative f,
    Aeson.FromJSON params,
    Aeson.ToJSON errorData,
    Aeson.ToJSON resultData
  ) =>
  (Maybe RequestId -> params -> f (Either (Error errorData) resultData)) ->
  t (Request Aeson.Value) ->
  f (t (Response Aeson.Value Aeson.Value))
traverseBatch f = wither $ \Request {..} ->
  case Aeson.parseEither (Aeson.parseJSON @params) params of
    Left err -> invalidParams err
    Right requestParams -> do
      outcome <- f id requestParams
      pure $ case outcome of
        Left err ->
          Just $
            Response
              { id = fromMaybe RequestIdNull id,
                outcome = Left $ Aeson.toJSON <$> err
              }
        Right res ->
          id <&> \reqId ->
            Response
              { id = reqId,
                outcome = Right $ Aeson.toJSON res
              }

-- methodNotFound = jsonRpcError MethodNotFound . ("Method not found: " <>)
-- invalidRequest = jsonRpcError InvalidRequest . Text.pack
invalidParams :: (Applicative f) => String -> f (Maybe (Response Aeson.Value Aeson.Value))
invalidParams = jsonRpcError InvalidParams . Text.pack

jsonRpcError :: (Applicative f) => ErrorCode -> Text -> f (Maybe (Response Aeson.Value Aeson.Value))
jsonRpcError code message =
  pure . Just $
    Response
      { -- We failed to parse but must report an error, spec says to
        -- report @null@ for @"id"@.
        id = RequestIdNull,
        outcome =
          Left $
            Aeson.toJSON @Aeson.Value
              <$> Error
                { code,
                  message,
                  data_ = Nothing
                }
      }
