module PKCloud.Core where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import Database.Esqueleto -- as Export
import Yesod.Auth
import Yesod.Core hiding (Value)

-- | Typeclass that each pkcloud application needs to implement. 
class PKCloudApp app where
    -- | App's display name.
    pkcloudAppName :: app -> Text

    -- | App's unique identifier. Must only be lowercase alphabetical characters and dashes (-).
    pkcloudAppIdentifier :: app -> Text

    -- | App's root route (or homepage). 
    pkcloudAppRoot :: Route app

    -- pkcloudAppIcon :: app -> () -- TODO: route to icon...
    -- pkcloudAppSummaryWidget :: app -> WidgetT app m ()

-- | Typeclass that final website needs to implement. 
class (GeneralPersistSql master (HandlerFor master), YesodAuth master, Eq (AuthId master), PersistField (AuthId master)) => PKCloud master where
--     -- Type classes to get user info???
--     -- name, ...
    pkcloudDefaultLayout :: (PKCloudApp app, ToMasterRoute app master) => app -> Text -> WidgetFor master () -> HandlerFor master Html
    pkcloudSetTitle :: Html -> WidgetFor master ()

    -- | Retrieve the display name for a user.
    pkcloudDisplayName :: AuthId master -> HandlerFor master Text

    -- | Retrieve the unique username for a user. 
    -- Usernames should be url safe.
    pkcloudUniqueUsername :: AuthId master -> HandlerFor master Text

    -- | Retrieve user for unique username.
    -- Usernames should be url safe.
    pkcloudLookupUniqueUsername :: Text -> HandlerFor master (Maybe (AuthId master))

    -- | Set a successful message. By default, just calls `setMessage`. 
    pkcloudSetMessageSuccess :: Text -> HandlerFor master ()
    pkcloudSetMessageSuccess = setMessage . toHtml

    -- | Set an info message. By default, just calls `setMessage`. 
    pkcloudSetMessageInfo :: Text -> HandlerFor master ()
    pkcloudSetMessageInfo = setMessage . toHtml

    -- | Set a warning message. By default, just calls `setMessage`. 
    pkcloudSetMessageWarning :: Text -> HandlerFor master ()
    pkcloudSetMessageWarning = setMessage . toHtml

    -- | Set a danger message. By default, just calls `setMessage`. 
    pkcloudSetMessageDanger :: Text -> HandlerFor master ()
    pkcloudSetMessageDanger = setMessage . toHtml

    -- | Specifies whether the given app is enabled for the given user. 
    pkcloudAppEnabled :: (PKCloudApp app) => app -> AuthId master -> HandlerFor master Bool
    pkcloudAppEnabled _ _ = return True

-- TODO: Move to Yesod.Core.
class ToMasterRoute child parent where
    toMasterRoute :: Route child -> Route parent

-- Note: Move GeneralPersist to a new library??
-- JP: Why is this needed? Can we drop it? XXX
-- | Generalized `runDB` for database transactions with any underlying monad. 
class Monad m => GeneralPersist site m | m -> site where
    type GeneralPersistBackend site
    runDB :: ReaderT (GeneralPersistBackend site) m a -> m a

-- class (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m) => GeneralPersistSql site m
type GeneralPersistSql site m = (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m)
