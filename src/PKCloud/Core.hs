module PKCloud.Core where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import Database.Esqueleto as Export
import Yesod.Auth
import Yesod.Core hiding (Value)

-- | Typeclass that each pkcloud application needs to implement. 
class PKCloudApp app where
    -- | App's display name.
    pkcloudAppName :: app -> Text

    -- | App's unique identifier. Should only be lowercase alphabetical characters and dashes (-).
    pkcloudAppIdentifier :: app -> Text

    -- pkcloudAppIcon :: app -> () -- TODO: route to icon...
    -- pkcloudAppSummaryWidget :: app -> WidgetT app m ()

-- | Typeclass that final website needs to implement. 
class (GeneralPersistSql master (HandlerT master IO), YesodAuth master, PersistField (AuthId master)) => PKCloud master where
--     -- Type classes to get user info???
--     -- name, ...
    pkcloudDefaultLayout :: (PKCloudApp app) => app -> Text -> WidgetT master IO () -> HandlerT master IO Html
    pkcloudSetTitle :: Html -> WidgetT master IO ()

    -- | Retrieve the display name for a user.
    pkcloudDisplayName :: AuthId master -> HandlerT master IO Text

    -- | Retrieve the unique username for a user. 
    -- Usernames should be url safe.
    pkcloudUniqueUsername :: AuthId master -> HandlerT master IO Text

    -- | Retrieve user for unique username.
    -- Usernames should be url safe.
    pkcloudLookupUniqueUsername :: Text -> HandlerT master IO (Maybe (AuthId master))

    -- | Set a successful message. By default, just calls `setMessage`. 
    pkcloudSetMessageSuccess :: Text -> HandlerT master IO ()
    pkcloudSetMessageSuccess = setMessage . toHtml

    -- | Set a danger message. By default, just calls `setMessage`. 
    pkcloudSetMessageDanger :: Text -> HandlerT master IO ()
    pkcloudSetMessageDanger = setMessage . toHtml

    -- | Specifies whether the given app is enabled for the given user. 
    pkcloudAppEnabled :: (PKCloudApp app) => app -> AuthId master -> HandlerT master IO Bool
    pkcloudAppEnabled _ _ = return True

-- TODO: Move to Yesod.Core.
class ToMasterRoute child parent where
    toMasterRoute :: Route child -> Route parent

-- Note: Move GeneralPersist to a new library??
-- | Generalized `runDB` for database transactions with any underlying monad. 
class Monad m => GeneralPersist site m | m -> site where
    type GeneralPersistBackend site
    runDB' :: ReaderT (GeneralPersistBackend site) m a -> m a

-- class (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m) => GeneralPersistSql site m
type GeneralPersistSql site m = (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m)
