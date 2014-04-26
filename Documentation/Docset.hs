module Documentation.Docset
       ( Docset()
       , DocsetInfo()
       , DocsetEntry()
       , openDocset
       , closeDocset
       , docsetInfo
       , docsetBaseDir
       , docsetEntries
       , countEntries
       , entriesForPattern
       , entryFullPath
       , module Documentation.Docset.EntryType
       ) where

import qualified Data.Text                      as T
import qualified Database.HDBC                  as DB
import qualified Database.HDBC.Sqlite3          as Sql3
import           Documentation.Docset.EntryType
import           Documentation.Docset.Info
import           Documentation.Docset.Layout

data QueryTable
  = QueryTable
    { countQuery :: String
    , allQuery :: String
    , likeQuery :: String
    }

data Docset
  = Docset
    { docsetBaseDir :: FilePath
    , docsetInfo    :: DocsetInfo
    , connection    :: Sql3.Connection
    , docsetQueryTable :: QueryTable
    }

data DocsetEntry
  = DocsetEntry
    { entryName :: T.Text
    , entryPath :: T.Text
    , entryType :: EntryType
    } deriving (Eq, Show)

openDocset :: FilePath -> IO Docset
openDocset basedir = do
  info <- parseInfoFile $ getInfoFilePath basedir
  conn <- Sql3.connectSqlite3 $ getDbPath basedir
  kind <- detectDbLayout conn
  return $ Docset basedir info conn kind

closeDocset :: Docset -> IO ()
closeDocset = DB.disconnect . connection

entryFullPath :: Docset -> DocsetEntry -> FilePath
entryFullPath ds e
  = documentPath (docsetBaseDir ds) (T.unpack $ entryPath e)

countEntries :: Docset -> IO Integer
countEntries ds = do
  let conn = connection ds
      query = countQuery $ docsetQueryTable ds
  [[n]] <- DB.quickQuery' conn query []
  return $ DB.fromSql n

docsetEntries :: Docset -> IO [DocsetEntry]
docsetEntries ds
  = runQuery ds allQuery []

entriesForPattern :: Docset -> String -> IO [DocsetEntry]
entriesForPattern ds pattern
  = runQuery ds likeQuery [DB.toSql pattern]

runQuery :: Docset
         -> (QueryTable -> String)
         -> [DB.SqlValue]
         -> IO [DocsetEntry]
runQuery ds queryAccessor params = do
  let query = queryAccessor $ docsetQueryTable ds
  entries <- DB.quickQuery (connection ds) query params
  return $ map toEntry entries

toEntry :: [DB.SqlValue] -> DocsetEntry
toEntry vals
  = DocsetEntry
    { entryName = DB.fromSql $ vals !! 0
    , entryType = textToEntryType $ DB.fromSql $ vals !! 1
    , entryPath = DB.fromSql $ vals !! 2
    }

dashBaseQuery :: String
dashBaseQuery = "select name, type, path from searchIndex"

zdashBaseQuery :: String
zdashBaseQuery
  = "select t.ztokenname as name\
   \, tt.ztypename as type\
   \, coalesce(tf.zpath || '#' || tm.zanchor, tf.zpath) as path\
   \ from ztoken t\
   \ join ztokentype tt on (t.ztokentype=tt.z_pk)\
   \ join ztokenmetainformation tm on (t.zmetainformation=tm.z_pk)\
   \ join zfilepath tf on (tm.zfile=tf.z_pk)"

defaultOrdering :: String
defaultOrdering = " order by name asc, type desc"

dashQueryTable :: QueryTable
dashQueryTable
  = QueryTable
    { countQuery = "select count(*) from searchIndex"
    , allQuery = dashBaseQuery ++ defaultOrdering
    , likeQuery = dashBaseQuery ++ " where name like ?" ++ defaultOrdering
    }

zdashQueryTable :: QueryTable
zdashQueryTable
  = QueryTable
    { countQuery = "select count(*) from ztoken"
    , allQuery = zdashBaseQuery ++ defaultOrdering
    , likeQuery = zdashBaseQuery ++ " where name like ?" ++ defaultOrdering
    }

detectDbLayout :: Sql3.Connection -> IO QueryTable
detectDbLayout c = lookupTable tables
  where tables = [("searchIndex", dashQueryTable), ("ZTOKENTYPE", zdashQueryTable)]
        lookupTable (x:xs) = do
          dsc <- DB.describeTable c $ fst x
          if null dsc
            then lookupTable xs
            else return $ snd x
        lookupTable _ = error "Unknown docset kind"
