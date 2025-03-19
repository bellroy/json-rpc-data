{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Json.Rpc.Endpoint
  ( -- * Creating Endpoints
    makeConcurrentEndpoint,
    makeEndpoint,

    -- * Creating Method Handlers
    MethodHandler (..),
    hoistMethodHandler,
    traverseBatchConcurrently,
    traverseBatch,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Functor (($>), (<&>))
import qualified Data.IntMap as IntMap
import Data.Json.Rpc.Endpoint.Internal
import Data.Json.Rpc.Types
  ( Error (..),
    Request (..),
    RequestId (..),
    Response (..),
  )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Internals.Async (Flat (..), FlatApp (..), runFlat)
import Witherable
import Prelude hiding (id)

makeConcurrentEndpoint ::
  forall m.
  (MonadUnliftIO m) =>
  Map Text (MethodHandler m) ->
  Aeson.Value ->
  m (Maybe Aeson.Value)
makeConcurrentEndpoint handlers value = withRunInIO $ \run ->
  let handlers' = hoistMethodHandler (FlatApp . FlatAction . run) <$> handlers
   in runFlat $ makeEndpoint handlers' value

makeEndpoint ::
  forall f.
  (Applicative f) =>
  Map Text (MethodHandler f) ->
  Aeson.Value ->
  f (Maybe Aeson.Value)
makeEndpoint handlers value = case value of
  Aeson.Bool {} ->
    pure . Just . Aeson.toJSON $
      invalidRequest "Expected array or object, got boolean"
  Aeson.Null {} ->
    pure . Just . Aeson.toJSON $
      invalidRequest "Expected array or object, got null"
  Aeson.Number {} ->
    pure . Just . Aeson.toJSON $
      invalidRequest "Expected array or object, got number"
  Aeson.String {} ->
    pure . Just . Aeson.toJSON $
      invalidRequest "Expected array or object, got string"
  Aeson.Object {} ->
    case Aeson.parseEither (Aeson.parseJSON @(Request Aeson.Value)) value of
      Left err -> pure . Just . Aeson.toJSON $ invalidRequest err
      Right req@Request {method} -> case Map.lookup method handlers of
        Nothing -> pure . Just . Aeson.toJSON $ methodNotFound method
        Just (MethodHandler f) -> fmap Aeson.toJSON <$> f (Just req)
  Aeson.Array {} ->
    case Aeson.parseEither (Aeson.parseJSON @(NonEmpty Aeson.Value)) value of
      Left err -> pure . Just . Aeson.toJSON $ invalidRequest err
      Right batch -> do
        let (invalidRequests, parsedRequests) =
              parseRequests . IntMap.fromList . zip [0 ..] $
                NonEmpty.toList batch

            byMethod = partitionByMethod parsedRequests
            applyHandler method methodBatch = case Map.lookup method handlers of
              Just (MethodHandler f) -> f methodBatch
              Nothing ->
                -- Every item in this batch should fail with "method not found".
                pure $ methodBatch $> methodNotFound method

        results <-
          IntMap.union invalidRequests . IntMap.unions . Map.elems
            <$> Map.traverseWithKey applyHandler byMethod

        -- If a batch is all notifications, and there were no errors,
        -- then we should return no result.
        --
        -- https://www.jsonrpc.org/specification#:~:text=all%20notifications
        --
        -- @
        -- --> [
        --         {"jsonrpc": "2.0", "method": "notify_sum", "params": [1,2,4]},
        --         {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]}
        --     ]
        -- <-- //Nothing is returned for all notification batches
        -- @
        pure $
          if IntMap.null results
            then Nothing
            else Just $ Aeson.toJSON results

-- | A 'MethodHandler' is given a
-- [batch](https://www.jsonrpc.org/specification#batch) of JSON-RPC
-- request records, and is expected to return the corresponding
-- error/success responses. It accepts a 'Witherable' because JSON-RPC
-- notifications should generate no response, and it is a GADT because
-- a handler must work for any 'Witherable'.
--
-- You are provided all the requests from a batch so that you can
-- consider the batch as a whole when planning to respond. For
-- example, you might want to prefetch relevant data for all requests
-- in a single database call, and refer to it as you handle each
-- request.
--
-- __NOTE:__ Every 'Witherable' is 'Traversable' and therefore
-- 'Foldable', so you can call 'Data.Foldable.toList' on the request
-- batch and inspect it as a list. You can also use tools from
-- "Control.Lens.Fold", if you're optically inclined.
--
-- If every request is completely independent, consider
-- 'traverseBatchConcurrently' to lift a handler for a single request
-- into a handler for batches. If requests must be processed
-- one-at-a-time, consider 'traverseBatch' instead.
data MethodHandler f where
  MethodHandler ::
    ( forall t.
      (Witherable t) =>
      t (Request Aeson.Value) ->
      f (t (Response Aeson.Value Aeson.Value))
    ) ->
    MethodHandler f

-- | Apply a natural transformation to a 'MethodHandler'.
--
-- This is useful if you want to change the applicative or monad your
-- handlers run in. You probably won't need this unless you're doing
-- fancy things with transformers or effect systems.
hoistMethodHandler ::
  (forall a. f a -> g a) -> MethodHandler f -> MethodHandler g
hoistMethodHandler nt (MethodHandler f) = MethodHandler (nt . f)

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
      (\reqId params -> FlatApp . FlatAction . run $ f reqId params)
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
    Left err -> pure . Just $ invalidParams err
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
