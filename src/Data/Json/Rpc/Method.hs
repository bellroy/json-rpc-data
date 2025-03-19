{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Json.Rpc.Method where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Constraint.Extras
import Data.Functor ((<&>))
import Data.Json.Rpc.Types
import Data.Kind (Type)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Some (Some (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Universe.Some (FiniteSome (..))
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude hiding (id)

-- | Fuse several "handlers" for JSON-RPC methods into a single function.
--
-- Think of this function as returning a function @(Aeson.Value -> m
-- (Maybe Aeson.Value))@. This library has no opinion about the
-- transport over which JSON-RPC is running.
--
-- The returned function holds a 'Data.Map.Map' to memoise the
-- association between JSON-RPC method names and the corresponding
-- handler functions. The final result is `Nothing` if the chosen
-- handler successfully processes a notification (a JSON-RPC request
-- with no request ID). Otherwise, it returns @Just@ a payload that
-- should be returned to the caller.
--
-- Uses of this function usually look something like this:
--
-- @
-- jsonRpcEndpoint :: 'Aeson.Value' -> IO (Maybe 'Aeson.Value')
-- jsonRpcEndpoint = runHandlers $ \case
--   Ping -> handlePing
--   Frobnicate -> handleFrobnicate
-- @
--
-- To do this, you must first do some pre-work to identify the methods
-- and their related response and error types. First, define 'Method'
-- instances for your JSON-RPC methods:
--
-- @
-- instance Method PingParams where
--   type Name PingParams = "ping"
--   type ResponseData PingParams = ()
--   type ErrorData PingParms = PingError
--
-- instance Method FrobnicateParams where
--   type Name FrobnicateParams = "frobnicate"
--   type ResponseData FrobnicateParams = FrobnicateResponseData
--
--   -- Frobnication never fails.
--   type ErrorData FrobnicateParams = Void
-- @
--
-- Second, implement the "handlers" for each method, which do the
-- actual work:
--
-- @
-- ping :: Maybe RequestId -> PingParams -> IO (Either (Error PingError) ())
-- frobnicate :: Maybe RequestId -> FrobnicateParams -> IO (Either (Error Void) FrobnicateResponseData)
-- @
--
-- Third, define a GADT to enumerate the methods you want to serve
-- together. It need a 'Data.Constraint.Extras.Has' instance (use
--  'Data.Constraint.Extras.TH.deriveArgDict') and a
--  'Data.Universe.Some.FiniteSome' instance (use
--  'Data.Universe.Some.TH.deriveUniverseSome' and a standalone
-- deriving declaration):
--
-- @
-- data Methods a where
--   Ping :: Methods PingRequest
--   Frobnicate :: Methods FrobnicateRequest
--
-- \$(deriveArgDict ''Methods)
-- \$(deriveUniverseSome ''Methods)
-- deriving anyclass instance FiniteSome Methods
-- @
runHandlers ::
  forall tag m.
  (Has Method tag, FiniteSome tag, Monad m) =>
  ( forall requestParams.
    tag requestParams ->
    Maybe RequestId ->
    requestParams ->
    m (Either (Error (ErrorData requestParams)) (ResponseData requestParams))
  ) ->
  Aeson.Value ->
  m (Maybe Aeson.Value)
runHandlers selectHandler =
  let handlers :: Map Text (Request Aeson.Value -> m (Maybe Aeson.Value))
      handlers = foldl' addHandler Map.empty (universeFSome @tag)
        where
          addHandler m (Some (tag :: tag params)) =
            has @Method tag $
              let selectedHandler = runHandler $ selectHandler tag
               in Map.insert
                    (Text.pack . symbolVal $ Proxy @(Name params))
                    selectedHandler
                    m

      runHandler ::
        forall p.
        (Method p) =>
        ( Maybe RequestId ->
          p ->
          m (Either (Error (ErrorData p)) (ResponseData p))
        ) ->
        Request Aeson.Value ->
        m (Maybe Aeson.Value)
      runHandler handler Request {..} =
        case Aeson.parseEither (Aeson.parseJSON @p) params of
          Left err -> invalidParams err
          Right requestParams -> do
            outcome <- handler id requestParams
            let resp = id <&> \reqId -> Response {id = reqId, outcome}
            pure $ Aeson.toJSON @(ResponseFor p) <$> resp

      methodNotFound = jsonRpcError MethodNotFound . ("Method not found: " <>)
      invalidRequest = jsonRpcError InvalidRequest . Text.pack
      invalidParams = jsonRpcError InvalidParams . Text.pack

      jsonRpcError :: ErrorCode -> Text -> m (Maybe Aeson.Value)
      jsonRpcError code message =
        pure . Just $
          Aeson.toJSON @(Response Void Void)
            Response
              { -- We failed to parse but must report an error, spec says to
                -- report @null@ for @"id"@.
                id = RequestIdNull,
                outcome =
                  Left
                    Error
                      { code,
                        message,
                        data_ = Nothing
                      }
              }
   in \json ->
        case Aeson.parseEither Aeson.parseJSON json of
          Left err -> invalidRequest err
          Right genericRequest ->
            case Map.lookup (method genericRequest) handlers of
              Nothing -> methodNotFound (method genericRequest)
              Just handler -> handler genericRequest

-- | Instances of this class associate the parameter type of a
-- JSON-RPC method to its method name, its response data type, and to
-- its error detail type.
class
  ( KnownSymbol (Name requestParams),
    Aeson.FromJSON requestParams,
    Typeable requestParams,
    Aeson.ToJSON (ResponseData requestParams),
    Typeable (ResponseData requestParams),
    Aeson.ToJSON (ErrorData requestParams),
    Typeable (ErrorData requestParams)
  ) =>
  Method requestParams
  where
  -- | The name of the RPC method expected at the @"method"@ key of a
  -- request object.
  type Name requestParams :: Symbol

  -- | The response data corresponding to this request type.
  type ResponseData requestParams :: Type

  -- | If you don't return detailed error information, this can be
  -- 'Data.Void.Void'.
  type ErrorData requestParams :: Type

-- | Convenience alias describing the response type for a 'Method' instance.
type ResponseFor requestParams =
  Response (ErrorData requestParams) (ResponseData requestParams)
