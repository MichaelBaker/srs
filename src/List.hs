module List where

import Control.Applicative ((<$>), (<*>))
import Options.Applicative (subparser, command, info, progDesc, argument, metavar, str, helper)
import ListDatabase        (ListDatabase)

data ListCommand = View { listName :: String } deriving (Show)

viewOptions = View <$> argument str (metavar "LIST_NAME")

options = subparser
  ( command "view" (info (helper <*> viewOptions) $ progDesc "View the contents of a list")
  )

run :: ListDatabase -> ListCommand -> IO ListDatabase
run db (View _) = return db
