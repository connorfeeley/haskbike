-- | Miscellanous utilities for the client API. Mostly useful for REPL testing.

module API.ClientUtils
     ( fromRight'
     , unsafeQuery
     ) where

import           API.ClientLifted
import           API.ResponseWrapper ( ResponseWrapper, _respData )

import           AppEnv

import           Servant.Client      ( ClientM )


-- | Fetch from bikeshare API unsafely. Throws an error if the request fails to be decoded.
unsafeQuery :: ClientM (ResponseWrapper b) -> AppM b
unsafeQuery endpoint = do
  statusResponse <- runQueryM endpoint

  pure $ _respData (fromRight' statusResponse)

fromRight' :: Either l r -> r
fromRight' (Right x) = x
fromRight' _         = error "fromRight', given a Left"
