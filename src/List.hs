module List where

import Data.Monoid         ((<>))
import Control.Monad       (forM_)
import Control.Applicative ((<$>), (<*>), pure)
import Options.Applicative (subparser, auto, command, info, progDesc, argument, metavar, str, helper)
import ListDatabase        (ListDatabase, List(..), lists, listItems, listName)

data ListCommand = View       { listNameToShow :: String }
                 | Add        { listNameToAdd :: String, listItemToAdd :: String }
                 | CreateList { listNameToAdd :: String }
                 | Complete   { listNameToComplete :: String, index :: Integer }
                 | Lists
                 deriving (Show)

viewOptions     = View       <$> argument str (metavar "LIST_NAME")
addOptions      = Add        <$> argument str (metavar "LIST_NAME") <*> argument str (metavar "ITEM")
createOptions   = CreateList <$> argument str (metavar "LIST_NAME")
completeOptions = Complete   <$> argument str (metavar "LIST_NAME") <*> argument auto (metavar "ITEM_NUMBER")

options = subparser
  (  command "view"     (info (helper <*> viewOptions)     $ progDesc "View the contents of a list")
  <> command "add"      (info (helper <*> addOptions)      $ progDesc "Add an item to a list")
  <> command "create"   (info (helper <*> createOptions)   $ progDesc "Create a list")
  <> command "complete" (info (helper <*> completeOptions) $ progDesc "Complete an item")
  <> command "lists"    (info (helper <*> pure Lists)      $ progDesc "Show all list names")
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
run db (Complete name i) = do
  let listExists = any (\list -> listName list == name) (lists db)
  if listExists
    then return $ completeItem db name i
    else putStrLn ("The list '" ++ name ++ "' does not exist.") >> return db
run db Lists = do
  mapM_ putStrLn $ map listName (lists db)
  return db

addItem name item list = if listName list == name then list { listItems = (listItems list) ++ [item] } else list

printListItems items = mapM_ putStrLn outputLines
  where outputLines = map (pad padding) $ zip ([0..] :: [Integer]) items
        padding     = length $ show $ length items
        pad amount (i, text) = take (amount - (length $ show i)) (repeat ' ') ++ show i ++ "  " ++ text

completeItem db name i = db { lists = map (remove name i) (lists db) }
  where remove n j list = if listName list == n then list { listItems = itemsWithoutIndex (listItems list) (fromIntegral j) } else list
        itemsWithoutIndex items idx = take idx items ++ drop (idx + 1) items
