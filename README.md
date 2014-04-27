Documentation.Docset
====================

It's a simple library to read docsets used by the Dash application. It
supports both "dash" and "zdash" (used by XCode) docset layouts.

Usage
-----

Here is a simple example of library usage:

```haskell
import Control.Exception (bracket)
import qualified Documentation.Docset as DS

-- operations on docset could throw, so we should use bracket

example :: FilePath -> IO ()
example path = bracket (DS.open path) DS.close $ \ds -> do
  num <- DS.countEntries ds  -- count num of entries
  putStrLn $ "Num entries: " ++ (show num)
  putStrLn $ "Docset metadata: " ++ (show $ DS.docsetInfo ds)
```
