-- |

module API.BikeShare
     ( bikeshareBaseUrl
     ) where
import           Servant.Client

bikeshareBaseUrl :: BaseUrl
bikeshareBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"
