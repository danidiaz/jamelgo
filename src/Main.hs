{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import qualified Data.Text as T
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Core
import           System.IO
import           Control.Applicative
import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Jamelgo
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I

data App = App
    { _heist :: Snaplet (Heist App)
    , _jamelgo :: Snaplet Jamelgo
    }

$(makeLenses ''App)

instance HasHeist App where
    heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet defaultConfig app
    where
    routes :: [(ByteString, Handler App App ())]
    routes = [ ("",          serveDirectory "static")
             ]
    app :: SnapletInit App App
    app = makeSnaplet "app" "An snaplet example application." Nothing $ do
        h <- nestSnaplet "" heist $ heistInit "templates"
        j <- nestSnaplet "" jamelgo $ jamelgoInit
        addRoutes routes
        return $ App h j

