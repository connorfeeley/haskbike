{-# LANGUAGE OverloadedStrings #-}

module Haskbike.Server.Middleware.Timing
     ( RequestIdKey
     , mkRequestTimingMiddleware
     , requestIdHeader
     , requestIdToText
     ) where

import           Data.Aeson              ( (.=) )
import qualified Data.Aeson              as A
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               ( Text )
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Time.Clock.POSIX   ( getPOSIXTime )
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import qualified Data.Vault.Lazy         as V

import           GHC.Clock               ( getMonotonicTime )

import qualified Network.HTTP.Types      as H
import           Network.Wai             ( Middleware, Request, Response, mapResponseHeaders, queryString,
                                           rawPathInfo, requestHeaders, requestMethod, responseHeaders,
                                           responseStatus, vault )

import           System.IO               ( hFlush, stderr )


-- | Vault key used to attach the per-request UUID to a 'Network.Wai.Request'.
--
-- Downstream consumers (e.g. SQL logging) can fetch the request ID with
-- 'Data.Vault.Lazy.lookup'.
type RequestIdKey = V.Key Text


-- | Header name used to expose the request ID on the response.
requestIdHeader :: H.HeaderName
requestIdHeader = "X-Request-Id"


-- | Convert a UUID to the canonical hyphenated text form used in headers and logs.
requestIdToText :: UUID.UUID -> Text
requestIdToText = UUID.toText


-- | Build a Wai 'Middleware' that:
--
--   * generates a fresh UUID per request
--   * stores it in the request 'Vault' under the returned key
--   * attaches it to the response as 'X-Request-Id'
--   * logs a single-line JSON record per request to stderr containing
--     request ID, method, path, query string, status, response size
--     (when reported via @Content-Length@), duration in milliseconds,
--     user agent, and referer
--
-- The vault key is returned so other components (e.g. SQL logging) can
-- look up the same request ID inside handlers.
mkRequestTimingMiddleware :: IO (RequestIdKey, Middleware)
mkRequestTimingMiddleware = do
  key <- V.newKey
  pure (key, timing key)


timing :: RequestIdKey -> Middleware
timing key app req respond = do
  reqUuid <- UUID.nextRandom
  let reqId   = requestIdToText reqUuid
      req'    = req { vault = V.insert key reqId (vault req) }
      reqIdBs = TE.encodeUtf8 reqId

  t0 <- getMonotonicTime
  app req' $ \resp -> do
    let resp' = mapResponseHeaders ((requestIdHeader, reqIdBs) :) resp
    sent <- respond resp'
    t1 <- getMonotonicTime
    emitLog req' resp' (t1 - t0) reqId
    pure sent


emitLog :: Request -> Response -> Double -> Text -> IO ()
emitLog req resp dtSeconds reqId = do
  now <- getPOSIXTime
  let durationMs :: Double
      durationMs = dtSeconds * 1000
      headerVal h = lookup h (requestHeaders req)
      asText = fmap TE.decodeUtf8
      reportedSize = case lookup H.hContentLength (responseHeaders resp) of
        Just bs | Just n <- readBytes bs -> A.toJSON (n :: Integer)
        _                                -> A.Null
      payload = A.object
        [ "ts"          .= (realToFrac now :: Double)
        , "request_id"  .= reqId
        , "method"      .= TE.decodeUtf8 (requestMethod req)
        , "path"        .= TE.decodeUtf8 (rawPathInfo req)
        , "query"       .= renderQuery (queryString req)
        , "status"      .= H.statusCode (responseStatus resp)
        , "duration_ms" .= durationMs
        , "size_bytes"  .= reportedSize
        , "user_agent"  .= asText (headerVal H.hUserAgent)
        , "referer"     .= asText (headerVal H.hReferer)
        ]
  BS.hPutStr stderr (LBS.toStrict (A.encode payload <> "\n"))
  hFlush stderr


renderQuery :: H.Query -> Text
renderQuery [] = ""
renderQuery qs = TE.decodeUtf8 . LBS.toStrict . BB.toLazyByteString $ H.renderQueryBuilder True qs


readBytes :: BS.ByteString -> Maybe Integer
readBytes bs =
  case reads (T.unpack (TE.decodeUtf8 bs)) of
    [(n, "")] -> Just n
    _         -> Nothing
