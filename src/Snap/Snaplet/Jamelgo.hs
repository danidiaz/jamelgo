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
    }

findJavaExecutable :: MonadIO m => FilePath -> ErrorT String m JavaExe
findJavaExecutable path = do
    list <- liftIO (filterM doesFileExist files)
    case list of
        [] -> throwError $ "Not found " <> path
        javaExe:_ -> return $ JavaExe javaExe
    where
        files =  [combine path "bin/java"] <**> [(`addExtension` "exe"), id]

loadJavaExecutableMap :: (Functor m,MonadIO m)
                      => FilePath -> ErrorT String m (Map T.Text JavaExe)
loadJavaExecutableMap path = do
    jreJs <- liftIO $ B.readFile path
    jreMap <- ErrorT . return . eitherDecodeStrict' $ jreJs 
    traverse findJavaExecutable jreMap 

jamelgoInit :: SnapletInit b Jamelgo
jamelgoInit  = do
    makeSnaplet "jamelgo" "Jamelgo Snaplet" Nothing $ do
        result <- runErrorT $ do
            jreJsPath <- (</> "JREs.js") <$> lift getSnapletFilePath
            lift $ printInfo $ 
                "Loading JRE locations from: " <> T.pack jreJsPath 
            javaMap <- loadJavaExecutableMap jreJsPath
            return $ Jamelgo javaMap
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

