module PKCloud.Security where

import Database.Persist.TH
import Yesod.Core.Types

-- Switch to an arbitrary lattice??
data PermissionLevel = 
      PermissionLevelAdmin
    | PermissionLevelRead
    | PermissionLevelWrite
    
    deriving (Show, Read, Eq)
derivePersistField "PermissionLevel"

-- -- config/models
-- AccessControlList
--     -- Just the ID?
-- 
-- AccessControlListUser
--     user UserId
--     acl AccessControlListId
--     permission PermissionLevel
--     UniqueAccessControlListUser user acl
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

class PKCloudSecurityPermissions master a where
    pkcloudCanRead :: a -> HandlerT master IO Bool
    pkcloudCanWrite :: a -> HandlerT master IO Bool
    pkcloudCanCreate :: a -> HandlerT master IO Bool
    pkcloudCanAdmin :: a -> HandlerT master IO Bool

-- -- Render widget to display/update
-- renderPermissionWidget :: AccessControlListId -> Widget
-- renderPermissionWidget aclId = undefined
