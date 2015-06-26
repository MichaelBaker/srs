module ListDatabase where

data ListDatabase = ListDatabase { lists :: [List] } deriving (Show, Read)
data List         = List { listName :: String, listItems :: [String] } deriving (Show, Read)

emptyDatabase = ListDatabase { lists = [] }
