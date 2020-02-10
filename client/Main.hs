{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (Action(..), Model(..), UserInfo(..), initialModel, viewModel)
import Data.Proxy ( Proxy(..) )
import Control.Lens ( (^.), (+=), (-=), (.=), makeLenses )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( (<#), View, App(..), consoleLog, noEff )
import Miso.Html.Event (onInput)
import Miso.String (MisoString, fromMisoString, ms, pack)
import JavaScript.Web.XMLHttpRequest
import Data.Aeson (eitherDecodeStrict, encode)
import Data.JSString.Text
import qualified Data.JSString as JSS (pack)
import GHCJS.Types
import qualified Data.Text as T
import Data.Maybe (maybe)
import Servant.API
import Servant.Client.Ghcjs


main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = NoOp
    , model = initialModel currentURI
    , update = updateModel
    , view = viewModel
    , events = Miso.defaultEvents
    , subs = [ Miso.uriSub HandleURIChange ]
    , mountPoint = Nothing
    }


updateModel
    :: Action
    -> Model
    -> Miso.Effect Action Model
updateModel action m =
    case action of

      NoOp ->
        noEff m

      AddOne ->
        noEff m

      SubtractOne ->
        noEff m

      ChangeURI uri ->
          m <# do
            Miso.pushURI uri
            pure NoOp

      HandleURIChange uri ->
        m {_uri = uri } <# do
          pure NoOp

      RegisterUser -> 
        m <# do
          SetUserInfo <$> (registerUser m)

      SetUserInfo userInfo ->
        m { _userInfo = Just userInfo } <# do
          pure NoOp

      UpdateEmail email ->
        m { _email = Just email } <# do pure NoOp

      UpdatePassword password ->
        m { _password = Just password } <# do pure NoOp


registerUser :: Model -> IO UserInfo
registerUser m = do
  let 
    email =
      maybe "" id (_email m)

    password =
      maybe "" id (_password m)

    req = 
      Request
        { reqMethod = POST
        , reqURI = pack "http://localhost:3006/users"
        , reqLogin = Nothing
        , reqHeaders = []
        , reqWithCredentials = False
        , reqData = 
          FormData 
            [ ( "email" :: JSString
              , StringVal $ textToJSString (fromMisoString email)
              )
            , ( "password" :: JSString
              , StringVal $ textToJSString (fromMisoString password)
              )
            ]
        }

  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String UserInfo of
    Left s ->
      pure $ User Nothing

    Right j ->
      pure j

