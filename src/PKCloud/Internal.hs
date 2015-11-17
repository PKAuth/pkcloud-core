-- | Private internal functionality for PKCloud core and PKCloud apps. 

module PKCloud.Internal where

import Control.Monad.IO.Class
import qualified Data.Time.Clock as Time
import Database.Esqueleto as Export

-- | Type constraint for subsite persistent entities for convenience. 
type SubEntity e = (PersistEntity e, SqlBackend ~ PersistEntityBackend e)

-- | Data.Time.Clock's `getCurrentTime` lifted for convenience.
getCurrentTime :: (MonadIO m) => m Time.UTCTime
getCurrentTime = liftIO Time.getCurrentTime

