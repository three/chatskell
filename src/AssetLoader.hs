{-# LANGUAGE TemplateHaskell #-}

module AssetLoader (
    getAsset,
    printAssetDebugInfo
) where

import Data.FileEmbed
import Data.List (find, findIndex)

import qualified Data.ByteString.Char8 as C

getAsset :: [Char] -> Maybe (C.ByteString, C.ByteString)
getAsset fname = fmap fToA
    (find ((==fname).fst) assetFiles :: Maybe (FilePath,C.ByteString))

printAssetDebugInfo :: IO ()
printAssetDebugInfo = (putStrLn.show) assetFiles

fToA :: (FilePath, C.ByteString) -> (C.ByteString, C.ByteString)
fToA (path, contents) = (assetType path, contents)

assetFiles :: [(FilePath, C.ByteString)]
assetFiles = $(embedDir "static/")

assetType :: FilePath -> C.ByteString
assetType fname = C.pack $  case takeExtension fname of
    "txt"  -> "text/plain"
    "html" -> "text/html"
    "css"  -> "text/css"
    "js"   -> "application/javascript"
    "json" -> "application/json"
    otherwise -> "text/plain"

takeExtension :: FilePath -> [Char]
takeExtension fname = case findIndex (=='.') fname of
    Just n  -> takeExtension $ drop (n+1) fname
    Nothing -> fname
