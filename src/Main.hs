import Prelude hiding        (readFile)
import Options.Applicative   (subparser, command, info, progDesc, argument, metavar, auto, str, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>), pure)
import Data.Monoid           ((<>))
import System.FilePath.Posix (joinPath)
import System.Directory      (getHomeDirectory)
import System.IO.Strict      (readFile)

data Command  = Add    { addQuestion :: String, addAnswer :: String }
              | Remove { removeFactId :: Integer }
              | List
              deriving (Show)
data Database = Database { dbNextId :: Integer, dbFacts :: [Fact] } deriving (Read, Show)
data Fact     = Fact { factId :: Integer, factQuestion :: String, factAnswer :: String } deriving (Read, Show)

addOptions    = Add <$> argument str (metavar "QUESTION") <*> argument str (metavar "ANSWER")
removeOptions = Remove <$> argument auto (metavar "FACTID")
listOptions   = pure List

options = subparser
        (  command "add"    (info addOptions    $ progDesc "Adds a fact to study")
        <> command "remove" (info removeOptions $ progDesc "Removes a fact from the database")
        <> command "list"   (info listOptions   $ progDesc "List all facts")
        )

main = do
  home <- getHomeDirectory
  let dbPath = joinPath [home, ".srs-database"]
  database <- readFile dbPath
  execParser (info (helper <*> options) idm) >>= run (read database) >>= (writeFile dbPath . show)

run :: Database -> Command -> IO Database
run db List = do
  mapM_ print (dbFacts db)
  return db
run db (Add q a) = do
  let newFact = Fact (dbNextId db) q a
  return db { dbNextId = dbNextId db + 1, dbFacts = newFact : dbFacts db }
run db (Remove i) = do
  let newFacts = filter ((/= i) . factId) $ dbFacts db
  return db { dbFacts = newFacts }
