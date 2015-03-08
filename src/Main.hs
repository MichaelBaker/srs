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
confidenceMappings    = zip [0..] confidences
confidences           = [(minBound :: Confidence)..]
integerToConfidence i = case drop i confidences of [] -> Nothing; (a:_) -> Just a

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

wordWrap maxLen paddings s = unlines $ map (\(p, l) -> p ++ l) $ zip paddings $ toLines "" (words s)
  where toLines l []     = [l]
        toLines l (w:ws) = if length l + length w > maxLen
                             then l : toLines w ws
                             else toLines (unwords [l, w]) ws

run :: Database -> Command -> IO Database
run db List = do
  let maxLen             = 1 + (maximum $ map (length . show) confidences)
      spaces             = repeat ' '
      linePadding        = take (maxLen + 4) spaces
      paddedConfidence c = let len = length $ show c in show c ++ (take (maxLen - len) spaces)
      questionPaddings   = "Q: " : repeat linePadding
      answerPaddings     = (take maxLen spaces ++ "A: ") : repeat linePadding
      factToLine f = concat [ paddedConfidence (factConfidence f)
                            , (wordWrap 80 questionPaddings $ factQuestion f)
                            , (wordWrap 80 answerPaddings $ factAnswer f)
                            ]
  mapM_ (putStr . factToLine) (dbFacts db)
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
