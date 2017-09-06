module PKCloud.File where

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Database.Persist
import Yesod.Core.Types

-- import PKCloud.Security
-- 
-- class PKCloudSecurityPermissions master file => PKCloudFile master file | file -> master where
class PKCloudFile master file | file -> master where
    pkcloudFilePath :: file -> FilePath -- TODO: make file type family?
    pkcloudFileContentType :: file -> Text
    pkcloudUniqueFilePath :: FilePath -> Unique file


-- File
--  path FilePath -- Canonicalized, absolute, unique
--  UniqueFilePath path
--
--  group SecurityGroupId


-- open file :: file handler/conduit source/bytestring/etc
-- create file...
-- write file...

-- pkOpenFileSource :: PKCloudFile master file => file -> HandlerT master IO (Source (ResourceT IO) (Flush Builder))
pkOpenFileTypedContent403 :: PKCloudFile master file => file -> HandlerT master IO TypedContent
pkOpenFileTypedContent403 f = do

    let contentType = Text.encodeUtf8 $ pkcloudFileContentType f
    let path = pkcloudFilePath f
    let part = Nothing

    return $ TypedContent contentType $ ContentFile path part
    -- TODO:
    -- Catch IO
    -- Use sendFilePart
    -- TODO, support rest return type 403, 
