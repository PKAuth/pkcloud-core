module PKCloud.Core where

import PKCloud.Import

class PKCloudApp app where
    pkcloudAppName :: app -> Text
    pkcloudAppIcon :: app -> () -- TODO: route to icon...
    pkcloudAppSummaryWidget :: app -> WidgetT app m ()

-- class PKCloud master where
--     -- Type classes to get user info???
--     -- name, ...
--     pkcloudDefaultLayout :: WidgetT app (WidgetT master m ()) () -> HandlerT master m Html

