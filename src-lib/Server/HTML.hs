-- |

module Server.HTML
     ( HTMLLucid
     ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Proxy

import           Lucid

import           Network.HTTP.Media   ( MediaType, (//), (/:) )

import           Prelude              ()
import           Prelude.Compat

import           Servant              as S


data HTMLLucid
instance Accept HTMLLucid where
    contentType :: Proxy HTMLLucid -> MediaType
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender :: ToHtml a => Proxy HTMLLucid -> a -> BL.ByteString
    mimeRender _ = renderBS . toHtml


-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender :: Proxy HTMLLucid -> Html a -> BL.ByteString
    mimeRender _ = renderBS
