{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Common as C
import           Data.Proxy
import qualified Lucid                                as L
import qualified Lucid.Base                           as L
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant (Handler, JSON, Post, Raw, Server, Tagged(..), serve, serveDirectoryFileServer, (:>), (:<|>)(..) )
import Servant.API (ReqBody)
import qualified System.IO                            as IO
--import qualified Data.ByteString.Char8 as B
--import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Data.Text as T

import qualified Miso
import Miso ( View )

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Running on port 3006..."

    Wai.run 3006 $ Wai.logStdout $ compress app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI =
       UserApi
  :<|> StaticAPI
  :<|> (ServerRoutes
  :<|> Raw) -- This will show the 404 page for any unknown route

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View C.Action))`
type ServerRoutes
   = Miso.ToServerRoutes C.ViewRoutes HtmlPage C.Action

type StaticAPI = "static" :> Raw
type UserApi = "users" :> ReqBody '[JSON] C.NewUser :> Post '[JSON] C.UserInfo

userServer :: C.NewUser -> Handler C.UserInfo
userServer u = do
  liftIO $ print (T.pack "user post request: ")
  liftIO $ print u
  pure $ C.User (Just "foo")

userApi :: Proxy UserApi
userApi = Proxy

app :: Wai.Application
app =
    simpleCors $ serve (Proxy @ServerAPI)
        (    userServer
        :<|> static
        :<|> serverHandlers
        :<|> Tagged page404
        )
  where
    static :: Server StaticAPI
    static = serveDirectoryFileServer "static"

    serverHandlers :: Server ServerRoutes
    serverHandlers = homeServer :<|> flippedServer

    -- Alternative type:
    -- Servant.Server (ToServerRoutes C.Home HtmlPage C.Action)
    -- Handles the route for the home page, rendering C.homeView.
    homeServer :: Handler (HtmlPage (View C.Action))
    homeServer =
        pure $ HtmlPage $
          C.viewModel $
          C.initialModel C.homeLink

    -- Alternative type:
    -- Server (ToServerRoutes C.Flipped HtmlPage C.Action)
    -- Renders the /flipped page.
    flippedServer :: Handler (HtmlPage (View C.Action))
    flippedServer =
        pure $ HtmlPage $
          C.viewModel $
          C.initialModel C.flippedLink

    -- The 404 page is a Wai application because the endpoint is Raw.
    -- It just renders the page404View and sends it to the client.
    page404 :: Wai.Application
    page404 _ respond = respond $ Wai.responseLBS
        HTTP.status404 [("Content-Type", "text/html")] $
        L.renderBS $ L.toHtml C.page404View


-- | Represents the top level Html code. Its value represents the body of the
-- page.
newtype HtmlPage a = HtmlPage a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = do
        L.doctype_
        L.head_ $ do
          L.title_ "Miso isomorphic example"
          L.meta_ [L.charset_ "utf-8"]

          L.with (L.script_ mempty)
            [ L.makeAttribute "src" "/static/all.js"
            , L.makeAttribute "async" mempty
            , L.makeAttribute "defer" mempty
            ]

        L.body_ (L.toHtml x)


