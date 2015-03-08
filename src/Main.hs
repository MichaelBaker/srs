import Prelude hiding        (readFile)
import Options.Applicative   (subparser, command, info, progDesc, argument, metavar, auto, str, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>), pure)
import Data.Monoid           ((<>))
import System.FilePath.Posix (joinPath)
import System.Directory      (getHomeDirectory)
import System.IO.Strict      (readFile)

data Command    = Add    { addConfidence :: Int, addQuestion :: String, addAnswer :: String }
                | Remove { removeFactId :: Int }
                | List
                deriving (Show)
data Database   = Database { dbNextId :: Int, dbFacts :: [Fact] } deriving (Read, Show)
data Fact       = Fact { factConfidence :: Confidence, factId :: Int, factQuestion :: String, factAnswer :: String } deriving (Read, Show)
data Confidence = Unknown
                | LittleKnown
                | Known
                | WellKnown
                | Unforgettable
                deriving (Read, Show, Ord, Eq, Enum, Bounded)

confidenceMappings :: [(Int, Confidence)]
confidenceMappings    = zip [0..] [(minBound :: Confidence)..]
integerToConfidence i = case drop i [(minBound :: Confidence)..] of [] -> Nothing; (a:_) -> Just a

addOptions    = Add <$> argument auto (metavar "CONFIDENCE") <*> argument str (metavar "QUESTION") <*> argument str (metavar "ANSWER")
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
run db (Add c q a) = do
  case integerToConfidence c of
    Just c' -> do
      let newFact = Fact c' (dbNextId db) q a
      return db { dbNextId = dbNextId db + 1, dbFacts = newFact : dbFacts db }
    Nothing -> do
      putStrLn (show c ++ " is not a valid confidence level.")
      mapM_ (\(i, x) -> putStrLn $ show i ++ " | " ++ show x) confidenceMappings
      return db
run db (Remove i) = do
  let newFacts = filter ((/= i) . factId) $ dbFacts db
  return db { dbFacts = newFacts }
