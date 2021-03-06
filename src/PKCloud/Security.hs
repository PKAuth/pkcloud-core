module PKCloud.Security (
      PKCloudSecurityPermissions(..)
    , PermissionLevel(..)
    , PKCloudSecurityGroup(..)
    , PKCloudSecurityToSecurityGroup(..)
    , pkcloudRequireRead
    , pkcloudRequireWrite
    , pkcloudRequireCreate
    , SubEntitySecureBackend
    , pkSecurityGroupsToFilter
    ) where

import Control.Monad (when)
import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Yesod.Auth (AuthId, requireAuthId)
import Yesod.Core (permissionDenied)
import Yesod.Core.Types

import PKCloud.Internal
import PKCloud.Core

-- JP: Move this into the subsite??
class PKCloudSecurityPermissions master a where
    pkcloudCanRead :: a -> HandlerFor master Bool
    pkcloudCanWrite :: a -> HandlerFor master Bool
    pkcloudCanCreate :: a -> HandlerFor master Bool
    pkcloudCanAdmin :: a -> HandlerFor master Bool

-- | Requires that the current user can read `a`. 
-- Otherwise, returns 403 permission denied.
pkcloudRequireRead :: PKCloudSecurityPermissions master a => a -> HandlerFor master ()
pkcloudRequireRead a = do
    allowed <- pkcloudCanRead a
    when (not allowed) $ 
        permissionDenied "Permission Denied"

pkcloudRequireWrite :: PKCloudSecurityPermissions master a => a -> HandlerFor master ()
pkcloudRequireWrite a = do
    allowed <- pkcloudCanWrite a
    when (not allowed) $ 
        permissionDenied "Permission Denied"

pkcloudRequireCreate :: PKCloudSecurityPermissions master a => a -> HandlerFor master ()
pkcloudRequireCreate a = do
    allowed <- pkcloudCanCreate a
    when (not allowed) $ 
        permissionDenied "Permission Denied"

-- Switch to an arbitrary lattice??
data PermissionLevel = 
      PermissionLevelRead
    | PermissionLevelWrite
    | PermissionLevelAdmin
    deriving (Show, Read, Eq, Ord)
derivePersistField "PermissionLevel"

class (SubEntityBackend master (SecurityGroupMember master), SubEntityBackend master (SecurityGroup master), PKCloud master) => PKCloudSecurityGroup master where
    data SecurityGroup master
    data SecurityGroupMember master

    pkSecurityGroupName :: SecurityGroup master -> Text

    pkSecurityGroupMemberGroup :: SecurityGroupMember master -> Key (SecurityGroup master)
    pkSecurityGroupMemberMember :: SecurityGroupMember master -> AuthId master
    pkSecurityGroupMemberMemberField :: EntityField (SecurityGroupMember master) (AuthId master)
    pkSecurityGroupMemberPermission :: SecurityGroupMember master -> PermissionLevel
    pkSecurityGroupMemberPermissionField :: EntityField (SecurityGroupMember master) PermissionLevel
    pkSecurityGroupUniqueMember :: Key (SecurityGroup master) -> AuthId master -> Unique (SecurityGroupMember master)

    -- Returns the user's primary SG (User's default SG, typically only contains the user as an admin).
    getPrimarySecurityGroup :: AuthId master -> Key (SecurityGroup master)

    -- | Retrieves the list of security groups the given user has permission for. These have default implementations and you probably don't need to implement them.
    pkSecurityGroupReadGroups :: AuthId master -> HandlerFor master [Key (SecurityGroup master)]
    pkSecurityGroupReadGroups = pkSecurityGroupGroupsHelper $ FilterOr [pkSecurityGroupMemberPermissionField ==. PermissionLevelAdmin, pkSecurityGroupMemberPermissionField ==. PermissionLevelWrite, pkSecurityGroupMemberPermissionField ==. PermissionLevelRead]

    pkSecurityGroupWriteGroups :: AuthId master -> HandlerFor master [Key (SecurityGroup master)]
    pkSecurityGroupWriteGroups = pkSecurityGroupGroupsHelper $ FilterOr [pkSecurityGroupMemberPermissionField ==. PermissionLevelAdmin, pkSecurityGroupMemberPermissionField ==. PermissionLevelWrite]
    
    pkSecurityGroupCreateGroups :: AuthId master -> HandlerFor master [Key (SecurityGroup master)]
    pkSecurityGroupCreateGroups = pkSecurityGroupGroupsHelper $ FilterOr [pkSecurityGroupMemberPermissionField ==. PermissionLevelAdmin, pkSecurityGroupMemberPermissionField ==. PermissionLevelWrite]

    pkSecurityGroupAdminGroups :: AuthId master -> HandlerFor master [Key (SecurityGroup master)]
    pkSecurityGroupAdminGroups = pkSecurityGroupGroupsHelper $ pkSecurityGroupMemberPermissionField ==. PermissionLevelAdmin


pkSecurityGroupGroupsHelper :: PKCloudSecurityGroup master => Filter (SecurityGroupMember master) -> AuthId master -> HandlerFor master [Key (SecurityGroup master)]
pkSecurityGroupGroupsHelper filter userId = do
        groups <- runDB $ selectList [pkSecurityGroupMemberMemberField ==. userId, filter] []
        return $ map (pkSecurityGroupMemberGroup . entityVal) groups

pkSecurityGroupsToFilter :: PersistField typ => EntityField record typ -> [typ] -> Filter record
pkSecurityGroupsToFilter field groups = FilterOr $ map (field ==.) groups

-- -- config/models
-- SecurityGroup
--     name Text
-- 
-- SecurityGroupMember
--     group SecurityGroupId
--     member UserId
--     permission PermissionLevel
--     UniqueSecurityGroupMember group member



class PKCloudSecurityGroup master => PKCloudSecurityToSecurityGroup master a where
    -- Get the security group that manages the given type. 
    pkcloudToSecurityGroup :: a -> HandlerFor master (Key (SecurityGroup master))

instance PKCloudSecurityToSecurityGroup master a => PKCloudSecurityPermissions master a where
    pkcloudCanRead = pkcloudSecurityPermissionsHelper PermissionLevelRead
    pkcloudCanWrite = pkcloudSecurityPermissionsHelper PermissionLevelWrite
    pkcloudCanCreate = pkcloudSecurityPermissionsHelper PermissionLevelWrite
    pkcloudCanAdmin = pkcloudSecurityPermissionsHelper PermissionLevelAdmin

pkcloudSecurityPermissionsHelper :: (PKCloudSecurityToSecurityGroup site a) => PermissionLevel -> a -> HandlerFor site Bool
pkcloudSecurityPermissionsHelper rl a = do
        userId <- requireAuthId
        groupId <- pkcloudToSecurityGroup a
        memberM <- runDB $ getBy $ pkSecurityGroupUniqueMember groupId userId
        case memberM of
            Nothing ->
                return False
            Just member ->
                return $ pkSecurityGroupMemberPermission (entityVal member) >= rl
        
type SubEntitySecureBackend s e = (PKCloudSecurityToSecurityGroup s e, SubEntityBackend s e)

-- -- Render widget to display/update
-- renderPermissionWidget :: AccessControlListId -> Widget
-- renderPermissionWidget aclId = undefined
