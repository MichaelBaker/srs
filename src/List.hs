module List where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid         ((<>))
import Options.Applicative (subparser, command, info, progDesc, argument, metavar, str, helper)
import Control.Monad       (forM_)
import ListDatabase        (ListDatabase, List(..), lists, listItems, listName)

data ListCommand = View { listNameToShow :: String }
                 | Add  { listNameToAdd :: String, listItemToAdd :: String }
                 deriving (Show)

viewOptions = View <$> argument str (metavar "LIST_NAME")
addOptions  = Add  <$> argument str (metavar "LIST_NAME") <*> argument str (metavar "ITEM")

options = subparser
  (  command "view" (info (helper <*> viewOptions) $ progDesc "View the contents of a list")
  <> command "add"  (info (helper <*> addOptions)  $ progDesc "Add an item to a list")
  )

run :: ListDatabase -> ListCommand -> IO ListDatabase
run db (View name) = do
  forM_ (lists db) $ \list -> do
    if (listName list) == name
       then print (listItems list)
       else return ()
  return db
run db (Add name item) = do
  let listExists = any (\list -> listName list == name) (lists db)
  return $ if listExists
     then db { lists = map (addItem name item) (lists db) }
     else db { lists = List { listName = name, listItems = [item] } : (lists db) }

addItem name item list = if listName list == name then list { listItems = (listItems list) ++ [item] } else list
