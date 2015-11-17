module PKCloud.Security where

import Database.Persist.TH
-- import PKCloud.Import

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

class SecurityPermissions a where
    canRead :: a -> m Bool
    canWrite :: a -> m Bool
    canCreate :: a -> m Bool
    canAdmin :: a -> m Bool

-- -- Render widget to display/update
-- renderPermissionWidget :: AccessControlListId -> Widget
-- renderPermissionWidget aclId = undefined