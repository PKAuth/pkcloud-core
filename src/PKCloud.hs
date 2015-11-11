-- | User configurable site should typically import this module. 
module PKCloud (module Export) where

import PKCloud.Core as Export
import PKCloud.Security as Export


-- Notes:
--
-- Core PKCloud lib
--
--        ^
--        |
--        |
--
-- { PKCloud subsites }
--
--        ^
--        |
--        |
--        |
--
-- User configurable site
--
--
--
-- APIs:
--
-- - Core layouts
-- - Security/access control
--  - User management
-- - Logging?
-- - File manager (read/write to files)
-- - Daemon servers?
-- - ...
-- - Languages??
--
-- 
-- Apps:
--
-- - Todo list (sharing)
-- - Email
-- - Calendar
-- - Chat
-- - Bug tracker
-- - Wishlist
-- - Budgeter
-- - Blog
-- - Publication list
-- - Weather
