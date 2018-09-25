-- | Private internal functionality for PKCloud core and PKCloud apps. 

module PKCloud.Internal where

import Control.Monad.IO.Class
import Yesod.Persist.Core (YesodPersistBackend)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Time.Clock as Time
import Database.Esqueleto as Export hiding (Value)
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core (whamlet, HandlerSite, PathPiece)
import Yesod.Form.Fields (checkBoxField) -- , OptionList, radioField, optionsPairs)
import Yesod.Form.Types (Field(..), FormMessage, FieldSettings(..))

-- | Type constraint for subsite persistent entities for convenience. 
type SubEntity e = (PersistEntity e, SqlBackend ~ PersistEntityBackend e, PathPiece (Key e), ToJSON (Key e), FromJSON (Key e), ToBackendKey SqlBackend e)
type SubEntityBackend s e = (YesodPersistBackend s ~ PersistEntityBackend e, SubEntity e)

-- | Data.Time.Clock's `getCurrentTime` lifted for convenience.
getCurrentTime :: (MonadIO m) => m Time.UTCTime
getCurrentTime = liftIO Time.getCurrentTime


-- JP: Make a PKCloud.Form module?
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

withAutocapitalizeNone :: FieldSettings site -> FieldSettings site
withAutocapitalizeNone fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("autocapitalize", "none") : fsAttrs fs

withAutocorrectOff :: FieldSettings site -> FieldSettings site
withAutocorrectOff fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("autocorrect", "off") : fsAttrs fs

-- bootstrapRadioField :: (Eq a, RenderMessage site FormMessage) => HandlerT site IO (OptionList a) -> Field (HandlerT site IO) a
-- bootstrapRadioField l = selectFieldHelper undefined undefined undefined l
-- 
-- (radioField l)
--     { fieldView = \theId name attrs val req -> do
--         opts <- fmap olOptions $ handlerToWidget l
--         [whamlet|
--             $newline never
--             <div .radio>
--                 <label>
--                     <input id=#{theId} *{attrs} type="checkbox" name=#{name} value=yes :showVal id val:checked> #{label}
--             TODO
--         |]
--     }
-- 
-- bootstrapRadioFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg) => [(msg, a)] -> Field (HandlerT site IO) a
-- bootstrapRadioFieldList l = bootstrapRadioField . optionsPairs

