module PKCloud.Core where

import Control.Monad.Trans.Reader

import Data.Text (Text)
import Database.Esqueleto as Export
import Yesod.Auth
import Yesod.Core hiding (Value)

-- import PKCloud.Import

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

-- TODO: Move to Yesod.Core.
class ToMasterRoute child parent where
    toMasterRoute :: Route child -> Route parent

-- | Generalized `runDB` for database transactions with any underlying monad. 
class Monad m => GeneralPersist site m | m -> site where
    type GeneralPersistBackend site
    runDB' :: ReaderT (GeneralPersistBackend site) m a -> m a

-- class (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m) => GeneralPersistSql site m
type GeneralPersistSql site m = (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m)

-- class (PersistEntity e, SqlBackend ~ PersistEntityBackend e) => SubEntity e
type SubEntity e = (PersistEntity e, SqlBackend ~ PersistEntityBackend e)
