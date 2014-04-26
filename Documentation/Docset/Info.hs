module Documentation.Docset.Info
       ( DocsetInfo (..)
       , parseInfoFile
       ) where

import           Text.XML.HXT.Core (no, withSubstDTDEntities, withValidate)
import qualified Text.XML.Plist    as PL

data DocsetInfo
  = DocsetInfo
    { docsetName           :: String
    , docsetBundleId       :: String
    , docsetPlatformFamily :: String
    , docsetIsDash         :: Bool
    , docsetJsEnabled      :: Bool
    } deriving (Eq, Show)

parseInfoFile :: FilePath -> IO DocsetInfo
parseInfoFile path = do
  obj <- PL.readPlistFromFile [ withValidate no
                              , withSubstDTDEntities no
                              ] path
  return $ pobjectToInfo obj

pobjectToInfo :: PL.PlObject -> DocsetInfo
pobjectToInfo (PL.PlDict dict)
  = let find = flip lookup $ dict
        getString = toStr . find
        getBool = toBool . find

        toStr (Just (PL.PlString s)) = s
        toStr _ = ""
        toBool (Just (PL.PlBool b)) = b
        toBool _ = False
    in DocsetInfo
       { docsetName = getString "CFBundleName"
       , docsetBundleId = getString "CFBundleIdentifier"
       , docsetPlatformFamily = getString "DocSetPlatformFamily"
       , docsetIsDash = getBool "isDashDocset"
       , docsetJsEnabled = getBool "isJavaScriptEnabled"
       }
pobjectToInfo o = error $ "Bad docset Info file " ++ show o
