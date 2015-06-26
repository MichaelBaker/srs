module Database where

import qualified SrsDatabase  as Srs
import qualified ListDatabase as List

data Database = Database { srs  :: Srs.SrsDatabase
                         , list :: List.ListDatabase
                         } deriving (Read, Show)

emptyDatabase = Database { srs  = Srs.emptyDatabase
                         , list = List.emptyDatabase
                         }
