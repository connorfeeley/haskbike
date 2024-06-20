-- | This module defines the error page for the Haskbike server.

module Haskbike.Server.Page.ErrorPage
     ( ErrorPage (..)
     ) where

import           Control.Monad.Catch ( Exception )

import qualified Data.Text           as T
import           Data.UUID           ( toText )
import           Data.UUID.V5        ( generateNamed, namespaceOID )

import           Lucid


data ErrorPage where
    ErrorPage :: { contactEmail :: T.Text
                 , exception :: Exception e => e
                 } -> ErrorPage

instance ToHtml ErrorPage where
  toHtmlRaw = toHtml
  toHtml errorPage = do
    div_ [style_ "width:800px; margin:0 auto;"] $ do
      h1_ [style_ "text-align:center;"] "Server Error"
      div_ . p_ $
        "Oops, you broke the server!"
      div_ . p_ $
        "Send me an email at " <> emailLink <> " with the error code: " <> code_ [] (toHtml errorId) <> "."
    where
      email = contactEmail errorPage
      emailLink = a_ [ href_ ("mailto:" <> email <> "?" <> emailSubject <> "&" <> emailBody) ] (toHtml email)
      emailSubject = "subject=" <> "Toronto Bike Share Explorer Error Report"
      emailBody = "body=" <> "Error code: " <> errorId
      errorId = toText $ generateNamed namespaceOID []
