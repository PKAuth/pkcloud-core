module PKCloud.Core where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import Database.Esqueleto as Export
import Yesod.Auth
import Yesod.Core hiding (Value)

-- | Typeclass that each pkcloud application needs to implement. 
class PKCloudApp app where
    pkcloudAppName :: app -> Text
    pkcloudAppIcon :: app -> () -- TODO: route to icon...
    pkcloudAppSummaryWidget :: app -> WidgetT app m ()

-- | Typeclass that final website needs to implement. 
class (GeneralPersistSql master (HandlerT master IO), YesodAuth master) => PKCloud master where
--     -- Type classes to get user info???
--     -- name, ...
    pkcloudDefaultLayout :: WidgetT master IO () -> HandlerT master IO Html
    pkcloudSetTitle :: Html -> WidgetT master IO ()

    -- Retrieve the display name for a user.
    pkcloudDisplayName :: AuthId master -> HandlerT master IO Text

    -- Retrieve the unique username for a user. 
    pkcloudUniqueUsername :: AuthId master -> HandlerT master IO Text

    -- | Set a successful message. By default, just calls `setMessage`. 
    pkcloudSetMessageSuccess :: Text -> HandlerT master IO ()
    pkcloudSetMessageSuccess = setMessage . toHtml

    -- | Set a danger message. By default, just calls `setMessage`. 
    pkcloudSetMessageDanger :: Text -> HandlerT master IO ()
    pkcloudSetMessageDanger = setMessage . toHtml

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
