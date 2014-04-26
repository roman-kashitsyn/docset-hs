module Documentation.Docset.EntryType
       ( EntryType(..)
       , textToEntryType
       ) where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text  as T

data EntryType
  = Unknown
  | Attribute
  | Binding
  | Builtin
  | Callback
  | Category
  | Class
  | Command
  | Component
  | Constant
  | Constructor
  | Define
  | Delegate
  | Directive
  | Element
  | Entry
  | Enum
  | Error
  | Event
  | Exception
  | Field
  | File
  | Filter
  | Framework
  | Function
  | Global
  | Guide
  | Instance
  | Instruction
  | Interface
  | Keyword
  | Library
  | Literal
  | Macro
  | Method
  | Mixin
  | Module
  | Namespace
  | Notation
  | Object
  | Operator
  | Option
  | Package
  | Parameter
  | Procedure
  | Property
  | Protocol
  | Record
  | Resource
  | Sample
  | Section
  | Service
  | Struct
  | Style
  | Subroutine
  | Tag
  | Trait
  | Type
  | Union
  | Value
  | Variable
  deriving (Eq, Enum, Read, Show)

textToEntryType :: T.Text -> EntryType
textToEntryType text = fromMaybe Unknown $ M.lookup text nameToType

nameToType :: M.Map T.Text EntryType
nameToType = M.fromList $ extra ++ pairs
  where vals = enumFrom Attribute
        names = map (T.pack . show) vals
        pairs = zip names vals
        extra =
          [ (T.pack "Word", Keyword)
          , (T.pack "cat", Category)
          , (T.pack "cl", Class)
          , (T.pack "clm", Method)
          , (T.pack "func", Function)
          , (T.pack "instp", Instruction)
          , (T.pack "macro", Macro)
          , (T.pack "specialization", Type)
          , (T.pack "tdef", Type)
          ]
