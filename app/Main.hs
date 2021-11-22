{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Paths_tacer
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    dutTempalte  <- getDataFileName (T.unpack dutTemp) >>= TIO.readFile

    let prop = fillPropertiesTemplate cfg propTemplate
        tb   = fillTestbenchTemplate cfg dutTempalte testTemplate

    let dir            = "./" ++ T.unpack opID
        propertiesFile = dir ++ "/properties.json"
        inputFile      = dir ++ "/input.scs"
        gitignoreFile  = dir ++ "/.gitignore"
    
    createDirectory dir

    let foo = flip copyFile gitignoreFile

    getDataFileName gitTemp >>= (`copyFile` gitignoreFile)

    TIO.writeFile propertiesFile prop
    TIO.writeFile inputFile tb

  where propTemp = "resource/prop-template.json"
        testTemp = "resource/tb-template.scs"
        gitTemp  = "resource/ignore-template.git"
