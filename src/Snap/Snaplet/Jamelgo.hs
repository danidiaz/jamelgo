{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.Jamelgo where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Exception (throwIO)
import Data.Monoid
import Data.Map
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath
import System.Directory

import Snap.Snaplet

data Jamelgo = Jamelgo
    {   _jres :: Map T.Text FilePath
    }

findJavaExecutable :: FilePath -> IO (Maybe FilePath)
findJavaExecutable path = 
    let possibilities = [combine path "bin"] <**> [id, (`addExtension` "exe")]
    in listToMaybe <$> filterM doesFileExist possibilities

jamelgoInit :: SnapletInit b Jamelgo
jamelgoInit  = do
    makeSnaplet "jamelgo" "Jamelgo Snaplet" Nothing $ do
        path <- flip combine "JREs.js" <$> getSnapletFilePath
        printInfo $ "Loading JREs from: " <> T.pack path
        jres_js <- liftIO $ B.readFile path
        case eitherDecodeStrict' jres_js of
            Left errmsg -> liftIO $ throwIO $ userError errmsg
            Right jres' -> do 
                return $ Jamelgo jres'
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

