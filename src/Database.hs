module Database where

import qualified SrsDatabase as Srs

data Database = Database { srs :: Srs.SrsDatabase } deriving (Read, Show)

emptyDatabase = Database { srs = Srs.emptyDatabase }
