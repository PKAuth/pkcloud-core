module PKCloud.Security (
      PKCloudSecurityPermissions(..)
    , PermissionLevel(..)
    , PKCloudSecurityGroup(..)
    , PKCloudSecurityToSecurityGroup(..)
    ) where

import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Yesod.Auth (AuthId, requireAuthId)
import Yesod.Core.Types

import PKCloud.Internal
import PKCloud.Core

class PKCloudSecurityPermissions master a where
    pkcloudCanRead :: a -> HandlerT master IO Bool
    pkcloudCanWrite :: a -> HandlerT master IO Bool
    pkcloudCanCreate :: a -> HandlerT master IO Bool
    pkcloudCanAdmin :: a -> HandlerT master IO Bool

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
    pkSecurityGroupMemberPermission :: SecurityGroupMember master -> PermissionLevel
    pkSecurityGroupUniqueMember :: Key (SecurityGroup master) -> AuthId master -> Unique (SecurityGroupMember master)

    -- Returns the user's primary SG (User's default SG, typically only contains the user as an admin).
    getPrimarySecurityGroup :: AuthId master -> Key (SecurityGroup master)


-- class PKCloudSecurityAccessControlList master acl member where
--     accessControlListId :: acl -> Key acl
--     accessControlListName :: acl -> Text
-- 
--     -- Returns the user's primary ACL (User's default ACL, typically only contains the user as an admin).
--     getPrimaryACL :: UserId -> AccessControlListId


-- -- config/models
-- AccessControlList
--     name Text
-- 
-- AccessControlListUser
--     acl AccessControlListId
--     user UserId
--     permission PermissionLevel
--     UniqueAccessControlListUser user acl
-- 
--



-- Old...?
-- 
-- AccessControlListGroup
--     group GroupId
--     acl AccessControlListId
--     permission PermissionLevel
--     UniqueAccessControlListGroup group acl
-- 
-- User
--     identity Text
--     UniqueUserIdentity identity
-- 
-- Group
--     name Text
--     UniqueGroupName name
-- 
-- GroupMember
--     group GroupId
--     member UserId
--     UniqueGroupMember group member

class PKCloudSecurityGroup master => PKCloudSecurityToSecurityGroup master a where
    -- Get the security group that manages the given type. 
    pkcloudToSecurityGroup :: a -> HandlerT master IO (Key (SecurityGroup master))

instance PKCloudSecurityToSecurityGroup master a => PKCloudSecurityPermissions master a where
    pkcloudCanRead = pkcloudSecurityPermissionsHelper PermissionLevelRead
    pkcloudCanWrite = pkcloudSecurityPermissionsHelper PermissionLevelWrite
    pkcloudCanCreate = pkcloudSecurityPermissionsHelper PermissionLevelWrite
    pkcloudCanAdmin = pkcloudSecurityPermissionsHelper PermissionLevelAdmin

pkcloudSecurityPermissionsHelper :: (PKCloudSecurityToSecurityGroup site a) => PermissionLevel -> a -> HandlerT site IO Bool
pkcloudSecurityPermissionsHelper rl a = do
        userId <- requireAuthId
        groupId <- pkcloudToSecurityGroup a
        memberM <- runDB' $ getBy $ pkSecurityGroupUniqueMember groupId userId
        case memberM of
            Nothing ->
                return False
            Just member ->
                return $ pkSecurityGroupMemberPermission (entityVal member) >= rl
        

    
    

-- -- Render widget to display/update
-- renderPermissionWidget :: AccessControlListId -> Widget
-- renderPermissionWidget aclId = undefined
