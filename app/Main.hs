{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Paths_tacer
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Functor
import System.Environment
import System.Exit
import System.Directory

main :: IO ()
main = do
    rawCfg  <- getArgs >>= BS.readFile . head

    let cfg     = parseACEConfig rawCfg
        opID    = aceID cfg
        dutTemp = T.concat ["resource/", opID, "-template.scs"]

    propTemplate <- getDataFileName propTemp >>= TIO.readFile
    testTemplate <- getDataFileName testTemp >>= TIO.readFile
    cornTemplate <- getDataFileName cornTemp >>= TIO.readFile
    dutTemplate  <- getDataFileName (T.unpack dutTemp) >>= TIO.readFile

    let prop  = fillPropertiesTemplate cfg propTemplate
        tb    = fillTestbenchTemplate cfg dutTemplate testTemplate
        corns = mapCornerTemplates cfg cornTemplate

    let dir            = "./" ++ T.unpack opID
        propertiesFile = dir ++ "/properties.json"
        testFile       = dir ++ "/" ++ T.unpack opID ++ ".scs"
        gitignoreFile  = dir ++ "/.gitignore"
    
    createDirectory dir

    getDataFileName gitTemp >>= (`copyFile` gitignoreFile)

    TIO.writeFile propertiesFile prop
    TIO.writeFile testFile tb

    mapM_ (\(f,c) -> TIO.writeFile (dir ++ "/" ++ f) c) corns

  where propTemp = "resource/prop-template.json"
        testTemp = "resource/tb-template.scs"
        cornTemp = "resource/corner-template.scs"
        gitTemp  = "resource/ignore-template.git"
