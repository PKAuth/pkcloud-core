
module PKCloud (module Export) where

import qualified PKCloud.Core as Export
import qualified PKCloud.Security as Export


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
