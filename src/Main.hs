import Prelude hiding        (readFile)
import Options.Applicative   (subparser, command, info, progDesc, argument, metavar, auto, str, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>), pure)
import Data.Monoid           ((<>))
import System.FilePath.Posix (joinPath)
import System.Directory      (getHomeDirectory)
import System.IO.Strict      (readFile)
import Data.Time             (getCurrentTime, utctDay, toGregorian)

data Command    = Add    { addConfidence :: Int, addQuestion :: String, addAnswer :: String }
                | Remove { removeFactId :: Int }
                | List
                deriving (Show)
data Database   = Database { dbNextId :: Int, dbFacts :: [Fact] } deriving (Read, Show)
data Fact       = Fact { factConfidence :: Confidence, factStudyDate :: StudyDate, factId :: Int, factQuestion :: String, factAnswer :: String } deriving (Read, Show)
data Confidence = Unknown
                | LittleKnown
                | Known
                | WellKnown
                | Unforgettable
                deriving (Read, Show, Ord, Eq, Enum, Bounded)
data StudyDate  = StudyDate { studyYear :: Integer, studyMonth :: Int, studyDay :: Int } deriving (Read, Show)

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

wordWrap maxLen s = toLines "" (words s)
  where toLines l []     = [l]
        toLines l (w:ws) = if length l + length w > maxLen
                             then l : toLines w ws
                             else toLines (unwords [l, w]) ws

run :: Database -> Command -> IO Database
run db List = do
  let maxLen             = 1 + (maximum $ map (length . show) confidences)
      spaces             = repeat ' '
      linePadding        = take maxLen spaces
      paddedConfidence c = let len = length $ show c in show c ++ (take (maxLen - len) spaces)
      textLines f        = concat [(wordWrap 80 $ "Q: " ++ factQuestion f), (wordWrap 80 $ "A: " ++ factAnswer f)]
      paddedTime t       = let timeString = concat [show $ studyYear t, "-", show $ studyMonth t, "-", show $ studyDay t] in timeString ++ take (maxLen - length timeString) spaces
      finalLines f       = case textLines f of
                             []         -> [paddedConfidence (factConfidence f), paddedTime (factStudyDate f)]
                             (a:[])     -> [paddedConfidence (factConfidence f) ++ a, paddedTime (factStudyDate f)]
                             (a:b:rest) -> [paddedConfidence (factConfidence f) ++ a, paddedTime (factStudyDate f) ++ b] ++ map (\r -> linePadding ++ r) rest
      factToLine f = unlines $ finalLines f
  mapM_ (putStr . factToLine) (dbFacts db)
  return db
run db (Add c q a) = do
  case integerToConfidence c of
    Just c' -> do
      t <- getCurrentTime
      let (y, m, d) = toGregorian $ utctDay t
      let newFact = Fact c' (StudyDate y m d) (dbNextId db) q a
      return db { dbNextId = dbNextId db + 1, dbFacts = newFact : dbFacts db }
    Nothing -> do
      putStrLn (show c ++ " is not a valid confidence level.")
      mapM_ (\(i, x) -> putStrLn $ show i ++ " | " ++ show x) confidenceMappings
      return db
run db (Remove i) = do
  let newFacts = filter ((/= i) . factId) $ dbFacts db
  return db { dbFacts = newFacts }
