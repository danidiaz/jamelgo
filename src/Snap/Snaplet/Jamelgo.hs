{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Snap.Snaplet.Jamelgo where

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (forM)
import Control.Monad.Trans
import Control.Monad.Error hiding (forM)
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
     deriving Show

newtype JavaExe = JavaExe FilePath deriving Show

data ArgType = PlainArg T.Text
             | PathArg T.Text T.Text -- prefix-relpath 
     deriving Show

data Arg = Arg
    {
        _exclusive :: Maybe OS
    ,   _argType :: ArgType
    } deriving Show

instance FromJSON Arg where
    parseJSON (Object v) = do
         osMaybe <- v .:? "onlyon"
         os <- forM osMaybe $ \osstr -> case (osstr::String) of
             "windows" -> return Windows
             "linux" -> return Linux
             _ -> mempty
         val1 <- v .:? "plainarg"
         val2 <- v .:? "patharg"
         case (val1,val2) of
             (Just val1',Nothing) -> Arg os . PlainArg <$> parseJSON val1'
             (Nothing,Just val2') -> (\p -> Arg os . PathArg p) <$> 
                                     val2' .: "prefix" <*> 
                                     val2' .: "relpath"
             _ -> mempty
    parseJSON _ = mempty

data Service = Service 
    {
       _defaultJvmArgs :: String
    ,  _exeRelpath :: FilePath
    ,  _argRelpath :: FilePath
    ,  _arglist :: [Arg]  
    } deriving Show

instance FromJSON Service where
    parseJSON (Object v) = Service <$> 
                           v .: "jvmargs" <*> 
                           v .: "exerelpath" <*> 
                           v .: "argrelpath" <*> 
                           v .: "arglist"
    parseJSON _ = mempty

data Jamelgo = Jamelgo
    {  _os :: OS
    ,  _jres :: Map T.Text JavaExe
    ,  _servers :: Map T.Text Service
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


findServer :: (Functor m,MonadIO m) 
           => FilePath 
           -> ErrorT String m Service
findServer path = do
    does <- liftIO $ doesFileExist path
    unless does . throwError $ "Path " <> path <> " not found."
    eitherDecodeFromFile path 


jamelgoInit :: SnapletInit b Jamelgo
jamelgoInit  = do
    makeSnaplet "jamelgo" "Jamelgo Snaplet" Nothing $ do
        result <- runErrorT $ do
            theOS <- loadOS "OS.js"
            javaMap <- loadMap findJavaExecutable "JREs.js"
            serverMap <- loadMap findServer "servers.js"
            return $ Jamelgo theOS javaMap serverMap
        either (liftIO . throwIO . userError) return $ result

$(makeLenses ''Jamelgo)
$(makeLenses ''Service)


