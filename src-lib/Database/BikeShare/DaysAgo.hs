-- |

module Database.BikeShare.DaysAgo
     ( DaysAgo (..)
     , daysAgo_
     ) where

import           Data.Attoparsec.Text
import qualified Data.Text                     as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax

import           Servant.API


data DaysAgo where
  DaysAgo :: (Num a, Integral a, Show a, HasSqlValueSyntax PgValueSyntax a) => a -> DaysAgo

instance HasSqlValueSyntax PgValueSyntax DaysAgo where
  sqlValueSyntax (DaysAgo d) = sqlValueSyntax d

daysAgo_ :: QGenExpr ctxt Postgres s DaysAgo -> QGenExpr ctxt Postgres s UTCTime
daysAgo_ = customExpr_ (\offs -> "(NOW() - INTERVAL '" <> offs <> " DAYS')")

instance FromHttpApiData DaysAgo where
  parseUrlPiece :: T.Text -> Either T.Text DaysAgo
  parseUrlPiece p = case parseOnly (decimal :: Parser Integer) p of
    Left e  -> Left  (T.pack e)
    Right v -> Right (DaysAgo v)
  parseQueryParam = parseUrlPiece

instance ToHttpApiData DaysAgo where
  toUrlPiece (DaysAgo d) = (T.pack . show) d
  toQueryParam = toUrlPiece
