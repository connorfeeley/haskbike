-- |

module Haskbike.Database.CustomExpressions
     ( currentTimestampUtc_
     ) where
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax


-- | Postgres @NOW()@ function. Returns the server's timestamp in UTC.
-- nowUtc_ :: QExpr Postgres s UTCTime
-- nowUtc_ = QExpr (\_ -> PgExpressionSyntax (emit "NOW() at time zone 'UTC'"))

currentTimestampUtc_ :: QExpr Postgres s UTCTime
currentTimestampUtc_ = QExpr (\_ -> PgExpressionSyntax (emit "(CURRENT_TIMESTAMP at time zone 'utc')"))
