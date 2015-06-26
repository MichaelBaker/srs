module List where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid         ((<>))
import Options.Applicative (subparser, command, info, progDesc, argument, metavar, str, helper)
import Control.Monad       (forM_)
import ListDatabase        (ListDatabase, List(..), lists, listItems, listName)

data ListCommand = View       { listNameToShow :: String }
                 | Add        { listNameToAdd :: String, listItemToAdd :: String }
                 | CreateList { listNameToAdd :: String }
                 deriving (Show)

viewOptions   = View       <$> argument str (metavar "LIST_NAME")
addOptions    = Add        <$> argument str (metavar "LIST_NAME") <*> argument str (metavar "ITEM")
createOptions = CreateList <$> argument str (metavar "LIST_NAME")

options = subparser
  (  command "view"   (info (helper <*> viewOptions)   $ progDesc "View the contents of a list")
  <> command "add"    (info (helper <*> addOptions)    $ progDesc "Add an item to a list")
  <> command "create" (info (helper <*> createOptions) $ progDesc "Create a list")
  )

run :: ListDatabase -> ListCommand -> IO ListDatabase
run db (View name) = do
  forM_ (lists db) $ \list -> do
    if (listName list) == name
       then printListItems (listItems list)
       else return ()
  return db
run db (Add name item) = do
  let listExists = any (\list -> listName list == name) (lists db)
  if listExists
     then return db { lists = map (addItem name item) (lists db) }
     else putStrLn ("The list '" ++ name ++ "' does not exist. You can create it with the 'create' command.") >> return db
run db (CreateList name) = do
  let listExists = any (\list -> listName list == name) (lists db)
  if listExists
    then return db
    else return db { lists = List name [] : (lists db) }

addItem name item list = if listName list == name then list { listItems = (listItems list) ++ [item] } else list

printListItems items = mapM_ putStrLn outputLines
  where outputLines = map (pad padding) $ zip ([0..] :: [Integer]) items
        padding     = length $ show $ length items
        pad amount (i, text) = take (amount - (length $ show i)) (repeat ' ') ++ show i ++ "  " ++ text
