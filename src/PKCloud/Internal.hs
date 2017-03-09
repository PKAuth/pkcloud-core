-- | Private internal functionality for PKCloud core and PKCloud apps. 

module PKCloud.Internal where

import Control.Monad.IO.Class
import Yesod.Persist.Core (YesodPersistBackend)
import Data.Text (Text)
import qualified Data.Time.Clock as Time
import Database.Esqueleto as Export
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core (whamlet, HandlerSite)
import Yesod.Form.Fields (checkBoxField)
import Yesod.Form.Types (Field(..), FormMessage)

-- | Type constraint for subsite persistent entities for convenience. 
type SubEntity e = (PersistEntity e, SqlBackend ~ PersistEntityBackend e)
type SubEntityBackend s e = (YesodPersistBackend s ~ PersistEntityBackend e, SubEntity e)

-- | Data.Time.Clock's `getCurrentTime` lifted for convenience.
getCurrentTime :: (MonadIO m) => m Time.UTCTime
getCurrentTime = liftIO Time.getCurrentTime


-- https://gist.github.com/carymrobbins/590515bb8dfb48573527
bootstrapCheckBoxField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Bool
bootstrapCheckBoxField label = checkBoxField
    { fieldView = \theId name attrs val _ -> [whamlet|
        $newline never
        <div .checkbox style="margin: 0px;">
            <label>
                <input id=#{theId} *{attrs} type="checkbox" name=#{name} value=yes :showVal id val:checked> #{label}
        |]
    }
  where
    showVal = either $ const False
    -- style="margin-top: -20px; margin-bottom: 0">
