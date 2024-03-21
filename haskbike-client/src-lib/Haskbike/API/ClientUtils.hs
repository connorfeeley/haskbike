-- | Miscellanous utilities for the client Haskbike.API. Mostly useful for REPL testing.

module Haskbike.API.ClientUtils
     ( fromRight'
     , unsafeQuery
     ) where

import           Haskbike.API.ClientLifted
import           Haskbike.API.ResponseWrapper ( ResponseWrapper, _respData )
import           Haskbike.AppEnv

import           Servant.Client               ( ClientM )


-- | Fetch from bikeshare API unsafely. Throws an error if the request fails to be decoded.
unsafeQuery :: ClientM (ResponseWrapper b) -> AppM b
unsafeQuery endpoint = do
  statusResponse <- runQueryM endpoint

  pure $ _respData (fromRight' statusResponse)

fromRight' :: Either l r -> r
fromRight' (Right x) = x
fromRight' _         = error "fromRight', given a Left"
