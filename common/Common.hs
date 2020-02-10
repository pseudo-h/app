{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.Html
import Miso.String (MisoString, ms)
import qualified Network.URI as Network
import GHC.Generics


import Data.Aeson.Types


data NewUser = 
  NewUser
    { newUserEmail :: MisoString
    , newUserPassword :: MisoString
    }
    deriving ( Show, Generic )

instance ToJSON NewUser
instance FromJSON NewUser

data UserInfo =
  User
    { newUserId :: Maybe MisoString }
    deriving ( Generic, Show, Eq )

instance ToJSON UserInfo
instance FromJSON UserInfo

data Model
   = Model
     { _uri          :: !Network.URI
     , _counterValue :: !Int
     , _email :: !(Maybe MisoString)
     , _password :: !(Maybe MisoString)
     , _userInfo :: !(Maybe UserInfo)
     }
     deriving (Eq, Show)

initialModel :: Network.URI -> Model
initialModel uri =
    Model
    { _uri = uri
    , _counterValue = 0
    , _email = Nothing
    , _password = Nothing
    , _userInfo = Nothing
    }

data Action
  = NoOp
  | AddOne
  | RegisterUser
  | SubtractOne
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  | SetUserInfo !UserInfo
  | UpdateEmail !MisoString
  | UpdatePassword !MisoString
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

makeLenses ''Model

-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Action
viewModel m =
    case Miso.runRoute (Proxy @ViewRoutes) viewTree _uri m of
      Left _routingError -> page404View
      Right v -> v

-- Servant tree of view functions
-- Should follow the structure of ViewRoutes
viewTree
    ::      (Model -> View Action)
       :<|> (Model -> View Action)
viewTree = homeView :<|> flippedView

-- View function of the Home route
homeView :: Model -> View Action
homeView _m =
    div_ []
      [ div_
        []
        [ input_ 
          [type_ "email"
          , placeholder_ "Email"
          , onInput UpdatePassword
          ]
        , input_ 
          [type_ "password"
          , placeholder_ "Password"
          , onInput UpdateEmail
          ]
        , button_ [onClick RegisterUser] [text "Register"]
        ]
      , button_ [ onClick $ ChangeURI flippedLink ] [ text "Go to /flipped" ]
      ]

-- View function of the Home route
flippedView :: Model -> View Action
flippedView m =
    div_ []
      [ div_
        []
        [ button_ [ onClick AddOne ] [ text "+" ]
        , text $ ms $ show $ _counterValue m
        , button_ [ onClick SubtractOne ] [ text "-" ]
        ]
      , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
      ]

page404View :: View Action
page404View =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif

-- Network.URI that points to the flipped route
flippedLink :: Network.URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#endif
