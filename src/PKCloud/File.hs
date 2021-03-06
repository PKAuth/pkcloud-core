module PKCloud.File (Path, PKCloudFile(..), pkCreateFileFromUploadOrExisting, pkSendFile) where

-- import Control.DeepSeq (NFData)
import Control.Exception.Enclosed (catchIO) -- (catchDeep, asIOException)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.Persist
import Database.Persist.Sql (transactionUndo, SqlBackend)
import Network.Mime (defaultMimeLookup)
import qualified System.FilePath as File
import qualified System.Directory as System
import Yesod.Core (lift, liftHandler, MonadHandler, sendFile, addHeader)
import Yesod.Core.Types

import PKCloud.Core
import PKCloud.Internal
import PKCloud.Rest (get404)
import PKCloud.Security

type Path = Text


-- TODO: 
-- Canonicalize?
-- Atomic filesystem lock so threads don't step on eachother?
-- Add security (Can user write/read to the given path?).

class (SubEntity (PKFile master), PKCloudSecurityPermissions master (PKFile master)) => PKCloudFile master where
    type PKFile master = f | f -> master

    -- Creates a `PKFile master` given a filename, content type, and filepath.
    pkFile :: Text -> Text -> Path -> PKFile master -- JP: Do we want creator (UserId), group, creation date, or created by application as well?

    pkFileName :: PKFile master -> Text
    pkFileContentType :: PKFile master -> Text
    pkFilePath :: PKFile master -> Path

    -- | Get the filepath of the default folder to store files in. 
    -- May be useful to return different folders based on current user (`maybeAuthId`) or current application (`getYesod`).
    -- Return `Nothing` if there is no default folder.
    pkDefaultFileFolder :: PKCloudApp app => SubHandlerFor app master (Either Text Path)

    pkCreateFileFromUpload :: (PKCloudApp app, MonadBaseControl IO (SubHandlerFor app master)) => FileInfo -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))
    pkCreateFileFromUpload fileInfo = _catchIO $ do
        -- Get default folder.
        folderE <- lift pkDefaultFileFolder
        case folderE of
            Left e ->
                return $ Left e
            Right folder -> do
                let folderS = Text.unpack folder

                -- JP: There could be a race condition here (probably unlikely though).
                --
                -- Make filename valid and get filepath.
                let nameS = File.makeValid $ Text.unpack $ fileName fileInfo
                p <- liftIO $ getUnusedName folderS nameS 0
                
                -- Create file.
                _pkCreateFileFromUploadAtPath fileInfo p

        where 
            -- JP: Do we need to do validation on filename? XXX
            getUnusedName folder name c = do
                -- Adjust filename for count of existing files.
                let adjustedName = if c == 0 then 
                        name 
                      else
                        let (n, e) = File.splitExtension name in -- JP: replaceBaseName?
                        n ++ "-" ++ show c ++ e

                -- Check if file exists.
                let path = File.combine folder adjustedName
                exists <- System.doesPathExist path
                if exists then
                    getUnusedName folder name (c+1)
                else
                    return path

    
    pkCreateFileFromUploadAtPath :: MonadBaseControl IO (SubHandlerFor app master) => FileInfo -> Path -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))
    pkCreateFileFromUploadAtPath fileInfo path = _catchIO $ _pkCreateFileFromUploadAtPath fileInfo $ Text.unpack path

    pkCreateFileFromExisting :: MonadBaseControl IO (SubHandlerFor app master) => Path -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))
    pkCreateFileFromExisting path = _catchIO $ do
        -- Canonicalize path.
        path <- liftIO $ System.canonicalizePath $ Text.unpack path

        -- Make sure file exists.
        exists <- liftIO $ System.doesFileExist path
        if not exists then
            return $ Left $ "File not found (" <> Text.pack path <> ")."
        else do
            -- Create file.
            let name = Text.pack $ File.takeBaseName path
            let contentType = Text.decodeUtf8 $ defaultMimeLookup $ Text.pack $ File.takeFileName path
            let file = pkFile name contentType $ Text.pack path

            -- Insert file into database.
            _pkInsertFile file $ return . Right

-- JP: Can we remove the SQL constraint?
-- JP: What should the return type be? We're (probably) not returning JSON...
-- | Send a file as a response. Will return file parts if If-Range requested.
-- Warning: Does not do any authentication.
pkSendFile :: forall master a . (GeneralPersistSql master (HandlerFor master), PKCloudFile master) => Key (PKFile master) -> HandlerFor master a
pkSendFile fileId = do
    -- Get file info.
    file <- runDB @master $ get404 fileId
    let contentType = Text.encodeUtf8 $ pkFileContentType file
    let path = Text.unpack $ pkFilePath file

    -- JP: sendFile might already implement ranges.
    -- rangeM <- parseRangeHeader
    -- let send = case rangeM of
    --         Nothing -> sendFile
    --         Just (start, end) -> \c p -> sendFilePart c p start end
    -- TODO: Verify no buffer vulnerabilities in sendFilePart. XXX

    -- Set name.
    -- Reference: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/competition-winners/part-5
    addHeader "Content-Disposition" $ Text.concat [ "attachment; filename=\"", pkFileName file, "\""]

    -- Indicate that we accept ranges.
    addHeader "Accept-Ranges" "bytes"

    -- Send file.
    sendFile contentType path

    -- where
        -- TODO: 
        --  Return the array of ranges with the end being optional.
        --  Check the If-Range.
        -- parseRangeHeader = do
            

-- | Helper function that either creates a file from an uploaded FileInfo or from an existing file at the given path. 
-- If both an uploaded file and path are given, this function will attempt to move the uploaded file to the given path.
pkCreateFileFromUploadOrExisting :: (PKCloudApp app, PKCloudFile master, MonadBaseControl IO (SubHandlerFor app master)) => Maybe FileInfo -> Maybe Path -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))

-- If a file was uploaded and a filepath was provided, attempt to save file to given filepath.
pkCreateFileFromUploadOrExisting (Just file) (Just path) = pkCreateFileFromUploadAtPath file path

-- If a file was uploaded and a filepath was not provided, save file to a generated path.
pkCreateFileFromUploadOrExisting (Just file) Nothing = pkCreateFileFromUpload file

-- If a file was not uploaded and a filepath was provided, attempt to use the provided filepath.
pkCreateFileFromUploadOrExisting Nothing (Just path) = pkCreateFileFromExisting path

-- If a file was not uploaded and a filepath was not provided, fail with invalid request.
pkCreateFileFromUploadOrExisting Nothing Nothing = return $ Left "No upload or existing file was provided."

_catchIO :: (MonadBaseControl IO m, MonadHandler m) => ReaderT SqlBackend m (Either Text a) -> ReaderT SqlBackend m (Either Text a)
_catchIO m = catchIO m $ \e -> do
    -- Rollback transaction.
    transactionUndo

    -- TODO: Log exception. XXX
    
    -- Return exception.
    return $ Left $ Text.pack $ show e

_pkCreateFileFromUploadAtPath :: PKCloudFile master => FileInfo -> String -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))
_pkCreateFileFromUploadAtPath fileInfo path = do
    -- Canonicalize path.
    path <- liftIO $ System.canonicalizePath path

    -- Make sure path is valid.
    if not (File.isValid path) then
        return $ Left $ "Invalid filepath (" <> Text.pack path <> ")."
    else do
        -- Check if file already exists.
        exists <- liftIO $ System.doesPathExist path
        if exists then
            return $ Left $ "Filepath already exists (" <> Text.pack path <> ")."
        else do

            -- Create file and insert it into DB.
            let file = pkFile (fileName fileInfo) (fileContentType fileInfo) $ Text.pack path
            _pkInsertFile file $ \key -> do
                -- Create directory if it doesn't exist.
                liftIO $ System.createDirectoryIfMissing True $ File.dropFileName path

                -- Move file.
                liftIO $ fileMove fileInfo path

                return $ Right key

_pkInsertFile :: PKCloudFile master => PKFile master -> (Key (PKFile master) -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))) -> ReaderT SqlBackend (SubHandlerFor app master) (Either Text (Key (PKFile master)))
_pkInsertFile file cps = do
    -- Check for create permission.
    lift $ liftHandler $ pkcloudRequireCreate file

    -- Insert into DB. 
    keyM <- insertUnique file
    case keyM of
        Nothing ->
            return $ Left "Could not create file. It already is in use."
        Just key ->
            cps key
    


-- import PKCloud.Security
-- 
-- class PKCloudSecurityPermissions master file => PKCloudFile master file | file -> master where
-- class PKCloudFile master file | file -> master where
--     pkcloudFilePath :: file -> FilePath -- TODO: make file type family?
--     pkcloudFileContentType :: file -> Text
--     pkcloudUniqueFilePath :: FilePath -> Unique file


-- File
--  path FilePath -- Canonicalized, absolute, unique
--  UniqueFilePath path
--
--  group SecurityGroupId


-- open file :: file handler/conduit source/bytestring/etc
-- create file...
-- write file...

-- pkOpenFileSource :: PKCloudFile master file => file -> HandlerT master IO (Source (ResourceT IO) (Flush Builder))
-- pkOpenFileTypedContent403 :: PKCloudFile master file => file -> HandlerT master IO TypedContent
-- pkOpenFileTypedContent403 f = do
-- 
--     let contentType = Text.encodeUtf8 $ pkcloudFileContentType f
--     let path = pkcloudFilePath f
--     let part = Nothing
-- 
--     return $ TypedContent contentType $ ContentFile path part
--     -- TODO:
--     -- Catch IO
--     -- Use sendFilePart
--     -- TODO, support rest return type 403, 
