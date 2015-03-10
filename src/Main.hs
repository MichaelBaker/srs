import Prelude hiding        (readFile, getLine)
import Options.Applicative   (subparser, command, info, progDesc, argument, metavar, auto, str, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>), pure)
import Data.Monoid           ((<>))
import System.FilePath.Posix (joinPath)
import System.Directory      (getHomeDirectory)
import System.IO.Strict      (readFile)
import Data.Time             (getCurrentTime, utctDay, toGregorian, fromGregorian, addDays)
import Data.List             (sortBy)
import Data.Ord              (comparing)
import Safe                  (readMay)
import System.IO             (getLine)

data Command    = Add    { addConfidence :: Int, addQuestion :: String, addAnswer :: String }
                | Remove { removeFactId :: Int }
                | Study
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
data StudyDate  = StudyDate { studyYear :: Integer, studyMonth :: Int, studyDay :: Int } deriving (Read, Show, Eq, Ord)

confidenceMappings :: [(Int, Confidence)]
confidenceMappings    = zip [0..] confidences
maxConfidenceInt      = fst $ head $ reverse confidenceMappings
confidences           = [(minBound :: Confidence)..]
integerToConfidence i = case drop i confidences of [] -> Nothing; (a:_) -> Just a

addOptions    = Add <$> argument auto (metavar "CONFIDENCE") <*> argument str (metavar "QUESTION") <*> argument str (metavar "ANSWER")
removeOptions = Remove <$> argument auto (metavar "FACTID")
listOptions   = pure List
studyOptions  = pure Study

options = subparser
        (  command "add"    (info addOptions    $ progDesc "Adds a fact to study")
        <> command "remove" (info removeOptions $ progDesc "Removes a fact from the database")
        <> command "list"   (info listOptions   $ progDesc "List all facts")
        <> command "study"  (info studyOptions  $ progDesc "Study todays facts")
        )

main = do
  home <- getHomeDirectory
  let dbPath = joinPath [home, ".srs-database"]
  database <- readFile dbPath
  newDb    <- execParser (info (helper <*> options) idm) >>= run (read database)
  writeFile dbPath $ show $ newDb { dbFacts = schedule (dbFacts newDb) }

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
      studyDate <- todaysStudyDate
      let newFact = Fact c' studyDate (dbNextId db) q a
      return db { dbNextId = dbNextId db + 1, dbFacts = newFact : dbFacts db }
    Nothing -> do
      putStrLn (show c ++ " is not a valid confidence level.")
      mapM_ (\(i, x) -> putStrLn $ show i ++ " | " ++ show x) confidenceMappings
      return db
run db (Remove i) = do
  let newFacts = filter ((/= i) . factId) $ dbFacts db
  return db { dbFacts = newFacts }
run db Study = do
  today <- todaysStudyDate
  let factsToStudy = takeWhile (\f -> factStudyDate f <= today) $ dbFacts db
      numFacts     = min (length factsToStudy) 10
  newFacts <- studyFacts numFacts today $ dbFacts db
  return db { dbFacts = newFacts }

studyFacts :: Int -> StudyDate -> [Fact] -> IO [Fact]
studyFacts 0 _ fs = putStrLn "Nothing more for today B)" >> return fs
studyFacts _ _ [] = putStrLn "There are no facts in your database" >> return []
studyFacts n today (f:fs)
  | factStudyDate f > today = putStrLn "Nothing more for today B)" >> return (f:fs)
  | otherwise = do
      putStrLn (factQuestion f)
      c <- getChar
      case c of _ -> return ()
      putStrLn (factAnswer f)
      confidence <- getConfidence
      let f' = f { factConfidence = confidence, factStudyDate = nextRepitition today confidence }
      studyFacts (n - 1) today (fs ++ [f'])

nextRepitition today Unknown       = incDate today 1
nextRepitition today LittleKnown   = incDate today 3
nextRepitition today Known         = incDate today 7
nextRepitition today WellKnown     = incDate today 14
nextRepitition today Unforgettable = incDate today 30

getConfidence :: IO Confidence
getConfidence = do
  putStrLn $ "How well did you know that?(0-" ++ show maxConfidenceInt ++ ")"
  c <- getLine
  case readMay c of
    Nothing -> putStrLn (show c ++ " is not a valid confidence level. Please enter an integer between 0 and " ++ show maxConfidenceInt) >> getConfidence
    Just a  -> case integerToConfidence a of
                 Nothing -> putStrLn (show c ++ " is not a valid confidence level. Please enter an integer between 0 and " ++ show maxConfidenceInt) >> getConfidence
                 Just b  -> return b

todaysStudyDate = do
  t <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay t
  return $ StudyDate y m d

schedule []     = []
schedule (f:fs) = shuffle facts (factStudyDate f) [] []
  where facts = sortBy (comparing factStudyDate) (f:fs)
        shuffle [] _ result working = result ++ working
        shuffle (a:as) date result working
          | length working == 9    = shuffle as (incDate (factStudyDate a) 1) (result ++ working ++ [a]) []
          | factStudyDate a < date = shuffle as date result (working ++ [a { factStudyDate = date }])
          | otherwise              = shuffle as date result (working ++ [a])

incDate (StudyDate y m d) days = let (y', m', d') = toGregorian $ addDays days $ fromGregorian y m d in StudyDate y' m' d'
