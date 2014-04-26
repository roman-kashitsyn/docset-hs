module Documentation.Docset.Layout
       ( getDbPath
       , getDocPath
       , getInfoFilePath
       , documentPath
       ) where

import           System.FilePath ((</>))

getDbPath :: FilePath -> FilePath
getDbPath basedir
  = basedir </> "Contents" </> "Resources" </> "docSet.dsidx"

getDocPath :: FilePath -> FilePath
getDocPath basedir
  = basedir </> "Contents" </> "Resources" </> "Documents"

getInfoFilePath :: FilePath -> FilePath
getInfoFilePath basedir
  = basedir </> "Contents" </> "Info.plist"

documentPath :: FilePath -> FilePath -> FilePath
documentPath basedir url
  = getDocPath basedir </> url
