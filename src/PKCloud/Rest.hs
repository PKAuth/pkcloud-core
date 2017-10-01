-- | Helper functions for creating JSON REST APIs.

module PKCloud.Rest (
      GetResponse
    , getResponse
    , getResponseNotFound
    , getResponseUnauthorized
    , getResponseBadRequest
    , PostResponse
    , postResponse
    , postResponseNotFound
    , postResponseConflict
    , postResponseUnauthorized
    , postResponseBadRequest

    , get404
    , getBy404
    , insert409
    , insert409_
    ) where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))
import qualified Data.Aeson as Aeson
import Data.Monoid ((<>))
import Database.Persist.Class (Key, PersistStore, PersistRecordBackend, PersistUnique, PersistUniqueWrite, Unique, get, getBy, insertUnique)
import Database.Persist.Types (Entity)
import Data.Text (Text)
import Network.HTTP.Types.Status (created201, badRequest400, unauthorized401, notFound404, conflict409)
import Yesod.Core (getUrlRender, addHeader, sendStatusJSON, MonadHandler, HandlerSite, Route)

-- JP: 
--  - We don't directly expose the constructors since ToTypedContent does not currently allow us to specify the response status or headers. Ideally, we return the constructors directly.
--  - It might be worth moving this to a separate library or integrate with yesod.
--  - Clients still need to consider potential short circuiting (ie get404, insert400, etc).
--  - Maybe the error message type could be generalized.
--  - Ideally these won't short circuit, but most of them do.

-- TODO
--  - Create patterns so that we can match on constructors but not create them.

-- | Response to GET requests.
data GetResponse a = 
      GetResponse a
    | GetResponseNotFound
    | GetResponsePermissionDenied
    | GetResponseUnauthorized
    | GetResponseBadRequest Text -- Error message.

instance ToJSON a => ToJSON (GetResponse a) where
    toJSON (GetResponse x) = Aeson.object [
          "status" .= (200 :: Int)
        , "data" .= x
        ]
    toJSON GetResponseNotFound = Aeson.object [
          "status" .= (404 :: Int)
        , "error" .= ("Not found" :: Text)
        ]
    toJSON GetResponsePermissionDenied = Aeson.object [
          "status" .= (403 :: Int)
        , "error" .= ("Permission denied" :: Text)
        ]
    toJSON GetResponseUnauthorized = Aeson.object [
          "status" .= (401 :: Int)
        , "error" .= ("Unauthorized" :: Text)
        ]
    toJSON (GetResponseBadRequest msg) = Aeson.object [
          "status" .= (400 :: Int)
        , "error" .= msg
        ]

instance FromJSON a => FromJSON (GetResponse a) where
    parseJSON (Aeson.Object o) = do
        status :: Int <- o .: "status"
        case status of
            200 -> do
                x <- o .: "data"
                return $ GetResponse x
            404 ->
                return GetResponseNotFound
            403 ->
                return GetResponsePermissionDenied
            401 -> 
                return GetResponseUnauthorized
            400 -> do
                msg <- o .: "error"
                return $ GetResponseBadRequest msg
            s -> fail $ "GetResponse: Invalid status code (" <> show s <> ")"
    parseJSON _ = fail "GetResponse: Not a JSON object"

getResponse :: (Monad m, ToJSON a) => a -> m (GetResponse a)
getResponse a = return $ GetResponse a

getResponseNotFound :: forall m a . (ToJSON a, MonadHandler m) => m (GetResponse a)
getResponseNotFound = sendStatusJSON notFound404 (GetResponseNotFound :: GetResponse a)
    -- Use notFound?

getResponseUnauthorized :: forall m a . (ToJSON a, MonadHandler m) => m (GetResponse a)
getResponseUnauthorized = sendStatusJSON unauthorized401 (GetResponseUnauthorized :: GetResponse a)

getResponseBadRequest :: forall m a . Text -> (ToJSON a, MonadHandler m) => m (GetResponse a)
getResponseBadRequest msg = sendStatusJSON badRequest400 (GetResponseBadRequest msg :: GetResponse a)


-- JP: 
--  insert400 -> insert409?

data PostResponse a = 
      PostResponse a
    | PostResponseNotFound
    | PostResponsePermissionDenied
    | PostResponseConflict
    | PostResponseUnauthorized
    | PostResponseBadRequest Text

instance ToJSON a => ToJSON (PostResponse a) where
    toJSON (PostResponse x) = Aeson.object [
          "status" .= (201 :: Int)
        , "data" .= x
        ]
    toJSON PostResponseNotFound = Aeson.object [
          "status" .= (404 :: Int)
        , "error" .= ("Not found" :: Text)
        ]
    toJSON PostResponsePermissionDenied = Aeson.object [
          "status" .= (403 :: Int)
        , "error" .= ("Permission denied" :: Text)
        ]
    toJSON PostResponseConflict = Aeson.object [
          "status" .= (409 :: Int)
        , "error" .= ("Conflict" :: Text)
        ]
    toJSON PostResponseUnauthorized = Aeson.object [
          "status" .= (401 :: Int)
        , "error" .= ("Unauthorized" :: Text)
        ]
    toJSON (PostResponseBadRequest msg) = Aeson.object [
          "status" .= (400 :: Int)
        , "error" .= msg
        ]

instance FromJSON a => FromJSON (PostResponse a) where
    parseJSON (Aeson.Object o) = do
        status :: Int <- o .: "status"
        case status of
            201 -> do
                x <- o .: "data"
                return $ PostResponse x
            404 -> 
                return PostResponseNotFound
            403 -> 
                return PostResponsePermissionDenied
            409 ->
                return PostResponseConflict
            401 ->
                return PostResponseUnauthorized
            400 -> do
                msg <- o .: "error"
                return $ PostResponseBadRequest msg
            s -> fail $ "PostResponse: Invalid status code (" <> show s <> ")"
    parseJSON _ = fail "PostResponse: Not a JSON object"
        

postResponse :: forall m a . (ToJSON a, MonadHandler m) => (a -> Route (HandlerSite m)) -> a -> m (PostResponse a)
postResponse r x = do
    render <- getUrlRender
    addHeader "Location" $ render $ r x
    sendStatusJSON created201 $ PostResponse x

postResponseNotFound :: forall m a . (ToJSON a, MonadHandler m) => m (PostResponse a)
postResponseNotFound = sendStatusJSON notFound404 (PostResponseNotFound :: PostResponse a)

postResponseConflict :: forall m a . (ToJSON a, MonadHandler m) => m (PostResponse a)
postResponseConflict = sendStatusJSON conflict409 (PostResponseConflict :: PostResponse a)

postResponseUnauthorized :: forall m a . (ToJSON a, MonadHandler m) => m (PostResponse a)
postResponseUnauthorized = sendStatusJSON unauthorized401 (PostResponseUnauthorized :: PostResponse a)

postResponseBadRequest :: forall m a . (ToJSON a, MonadHandler m) => Text -> m (PostResponse a)
postResponseBadRequest msg = sendStatusJSON badRequest400 (PostResponseBadRequest msg :: PostResponse a)


-- Database functions that provide REST responses on failure.
-- 
-- Note: These functions depend on the short circuiting of the functions above. 
-- 

get404 :: (MonadHandler m, PersistStore backend, PersistRecordBackend val backend) => Key val -> ReaderT backend m val
get404 k = do
    resM <- get k
    case resM of
        Just res ->
            return res
        Nothing -> do
            (_ :: GetResponse ()) <- getResponseNotFound
            error "unreachable"

getBy404 :: (PersistUnique backend, PersistRecordBackend val backend, MonadHandler m) => Unique val -> ReaderT backend m (Entity val)
getBy404 u = do
    resM <- getBy u
    case resM of
        Just res ->
            return res
        Nothing -> do
            (_ :: GetResponse ()) <- getResponseNotFound
            error "unreachable"

insert409 :: (MonadHandler m, PersistUniqueWrite backend, PersistRecordBackend val backend) => val -> ReaderT backend m (Key val)
insert409 v = do
    keyM <- insertUnique v
    case keyM of
        Just key ->
            return key
        Nothing -> do
            (_ :: PostResponse ()) <- postResponseNotFound
            error "unreachable"
    
insert409_ :: (MonadHandler m, PersistUniqueWrite backend, PersistRecordBackend val backend) => val -> ReaderT backend m ()
insert409_ v = insert409 v >> return ()

