{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Map
import Data.Maybe
import Data.Aeson
import Data.Traversable
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath
import System.Directory

import Snap.Snaplet

newtype JavaExe = JavaExe FilePath

data Jamelgo = Jamelgo
    {   _jres :: Map T.Text JavaExe
    ,   _servers :: Map T.Text FilePath
    }

eitherDecodeFromFile :: (Functor m,MonadIO m,FromJSON j)
                     => FilePath -> ErrorT String m (Map T.Text j)
eitherDecodeFromFile =
    liftIO . B.readFile >=> ErrorT . return . eitherDecodeStrict'

loadMap :: (FromJSON j) 
        => (j -> ErrorT String (Initializer b v) r)
        -> FilePath
        -> ErrorT String (Initializer b v) (Map T.Text r)  
loadMap traversal file = do
        path <- (</> file) <$> lift getSnapletFilePath
        lift $ printInfo $ 
            "Loading " <> T.pack file <> " from " <> T.pack path
        eitherDecodeFromFile path >>= traverse traversal 

findJavaExecutable :: MonadIO m => FilePath -> ErrorT String m JavaExe
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
            javaMap <- loadMap findJavaExecutable "JREs.js"
            serverMap <- loadMap return "servers.js"
            return $ Jamelgo javaMap serverMap
        either (liftIO . throwIO . userError) return $ result

--
--        TR.traverse printInfo $ Flip mapE
--        seed <- liftIO newStdGen 
--        now <- liftIO getCurrentTime 
--        let poemz = maybe M.empty id $ hush mapE :: M.Map Langname [Verse]
--            elemz = M.elems poemz -- discard the language names
--            langCount' = genericLength elemz
--            verseCount' = F.maximum . map genericLength $ elemz
--            filler = S.repeat . S.repeat $ "buffalo" 
--            verses = distribute $ compile filler elemz         
--            eternity' = Eternity langCount' verseCount' verses
--            (origin',mutations') = evalRand (futurify eternity') seed
--        snaplet <- StochasticText eternity' <$>
--                       (liftIO . newMVar $ Sempiternity now origin' mutations')
--        liftIO . forkIO $ langolier (milis 10000) 
--                                    (milis 30000)   
--                                    (snaplet^.sempiternity) 
--        return snaplet 
--

$(makeLenses ''Jamelgo)

