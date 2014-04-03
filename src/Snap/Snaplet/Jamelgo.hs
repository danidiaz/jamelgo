{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Snap.Snaplet.Jamelgo where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Lens
import Control.Exception (throwIO)
import Data.Monoid
import Data.Distributive
import Data.Char
import Data.Map
import qualified Data.Map as M
import Data.Maybe
import Data.Aeson
import Data.Traversable
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath
import System.Directory

import Snap.Snaplet

data OS = Linux
        | Windows

newtype JavaExe = JavaExe FilePath

data Service = Service 
    {
       _baseRelPath :: FilePath
    ,  _defaultJvmArgs :: String
    }

data Jamelgo = Jamelgo
    {  _os :: OS
    ,  _jres :: Map T.Text JavaExe
    ,  _servers :: Map T.Text FilePath
    }

eitherDecodeFromFile :: (Functor m,MonadIO m,FromJSON j)
                     => FilePath -> ErrorT String m j
eitherDecodeFromFile =
    liftIO . B.readFile >=> ErrorT . return . eitherDecodeStrict'

(???) :: Monad m => Maybe a -> e -> ErrorT e m a
(???) m e = ErrorT . return . maybe (Left e) Right $ m

loadOS :: FilePath  
       -> ErrorT String (Initializer b v) OS
loadOS file = do
    path <- (</> file) <$> lift getSnapletFilePath 
    lift $ printInfo $ 
        "Loading " <> T.pack file <> " from " <> T.pack path
    singletonMap <- eitherDecodeFromFile path
    str <- fmap T.toLower $ 
        M.lookup ("os"::T.Text) singletonMap ??? "No os entry!" 
    if | str == "windows" -> return Windows
       | str == "linux" -> return Linux
       | otherwise -> throwError $ 
            "Unable to determine OS from string: " <> show str

loadMap :: (FromJSON j) 
        => (j -> ErrorT String (Initializer b v) r)
        -> FilePath
        -> ErrorT String (Initializer b v) (Map T.Text r)  
loadMap traversal file = do
        path <- (</> file) <$> lift getSnapletFilePath
        lift $ printInfo $ 
            "Loading " <> T.pack file <> " from " <> T.pack path
        eitherDecodeFromFile path >>= traverse traversal 

findJavaExecutable :: (MonadIO m) 
                   => FilePath 
                   -> ErrorT String m JavaExe
findJavaExecutable path = do
    list <- liftIO (filterM doesFileExist files)
    case list of
        [] -> throwError $ "Not found " <> path
        javaExe:_ -> return $ JavaExe javaExe
    where
        files =  [combine path "bin/java"] <**> [(`addExtension` "exe"), id]

jamelgoInit :: SnapletInit b Jamelgo
jamelgoInit  = do
    makeSnaplet "jamelgo" "Jamelgo Snaplet" Nothing $ do
        result <- runErrorT $ do
            theOS <- loadOS "OS.js"
            javaMap <- loadMap findJavaExecutable "JREs.js"
            serverMap <- loadMap return "servers.js"
            return $ Jamelgo theOS javaMap serverMap
        either (liftIO . throwIO . userError) return $ result

$(makeLenses ''Jamelgo)
$(makeLenses ''Service)


