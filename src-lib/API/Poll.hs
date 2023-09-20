-- |

module API.Poll where

import           API.Types               ( StationInformationResponse
                                         , StationStatusResponse
                                         , StationStatusResponseData(..))
import           BikeShareAPI
import API.ResponseWrapper

import           API.Client
import           Control.Exception       (Exception (displayException))
import           Data.Aeson              (Object)
import           Data.Proxy
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           Text.Pretty.Simple      (pPrintString)

poll :: IO ()
poll = do
    clientManager <- mkClientManager

    --
    -- Example usage of API client functions.
    --
    status <- runQuery clientManager stationStatus
    case status of
        Left err       -> pPrintString $ displayException err
        Right response -> pPrintString $ show $ response_last_updated response

    pure ()
