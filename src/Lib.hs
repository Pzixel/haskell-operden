module Lib
    ( someFunc
    ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import System.IO
import Control.Lens       hiding ((.=))

type UserAPI = "users" :> Get '[JSON] [User]

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, ToSchema)

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

userAPI :: Proxy API
userAPI = Proxy

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> UserAPI

server :: Server API
server = swaggerSchemaUIServer swaggerDoc :<|> pure users

app :: Application
app = serve userAPI server

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy UserAPI)
    & info.title       .~ "Operden API"
    & info.version     .~ "1.0.0"
    & info.description ?~ "This is an API that perform some operen actions"

someFunc :: IO ()
someFunc = do
  print "Running"
  hFlush stdout
  run 8081 app
