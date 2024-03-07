-- |

module API.ClientUtils where

import           API.Client
import           API.ClientLifted
import           API.ResponseWrapper ( ResponseWrapper, _respData )
import           API.StationStatus   ( StationStatus )

import           AppEnv

import           Servant.Client      ( ClientError )



-- | Fetch station status unsafely.
unsafeQueryStationStatus :: AppM [StationStatus]
unsafeQueryStationStatus = do
  statusResponse <- runQueryM stationStatus :: AppM (Either ClientError (ResponseWrapper [StationStatus]))

  pure $ _respData (fromRight' statusResponse)

fromRight' :: Either l r -> r
fromRight' (Right x) = x
fromRight' _         = error "fromRight', given a Left"
