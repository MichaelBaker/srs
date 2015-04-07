import Prelude hiding        (readFile, getLine)
import Options.Applicative   (subparser, command, info, progDesc, argument, metavar, auto, str, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>), pure)
import Data.Monoid           ((<>))
import System.FilePath.Posix (joinPath)
import System.Directory      (getHomeDirectory)
import System.IO.Strict      (readFile)
import Data.Time             (getZonedTime, localDay, zonedTimeToLocalTime, toGregorian, fromGregorian, addDays)
import Safe                  (readMay)
import System.IO             (getLine)

data Command    = Add    { addConfidence :: Int, addQuestion :: String, addAnswer :: String }
                | Remove { removeFactId :: Int }
                | Study
                | List
                deriving (Show)
data Database   = Database { dbNextId :: Int, dbFacts :: [Fact], dbFinished :: [Fact] } deriving (Read, Show)
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
  writeFile dbPath $ show $ newDb

wordWrap maxLen s = toLines "" (words s)
  where toLines l []     = [l]
        toLines l (w:ws) = if length l + length w > maxLen
                             then l : toLines w ws
                             else toLines (unwords [l, w]) ws

run :: Database -> Command -> IO Database
run db List = do
  let maxLen               = 5 + (maximum $ map (length . show) confidences)
      spaces               = repeat ' '
      linePadding          = take maxLen spaces
      textLines f          = concat [(wordWrap 80 $ "Q: " ++ factQuestion f), (wordWrap 80 $ "A: " ++ factAnswer f)]
      paddedConfidence c i = let s = show i ++ ": " ++ show c in s ++ (take (maxLen - (length s)) spaces)
      paddedTime t         = let timeString = concat [show $ studyYear t, "-", show $ studyMonth t, "-", show $ studyDay t] in timeString ++ take (maxLen - length timeString) spaces
      finalLines f         = case textLines f of
                               []         -> [paddedConfidence (factConfidence f) (factId f), paddedTime (factStudyDate f)]
                               (a:[])     -> [paddedConfidence (factConfidence f) (factId f) ++ a, paddedTime (factStudyDate f)]
                               (a:b:rest) -> [paddedConfidence (factConfidence f) (factId f) ++ a, paddedTime (factStudyDate f) ++ b] ++ map (\r -> linePadding ++ r) rest
      factToLine f = unlines $ finalLines f
  mapM_ (putStr . (++ "\n") . factToLine) (dbFacts db)
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
  (newFacts, done) <- studyFacts numFacts today (dbFinished db) (dbFacts db)
  return db { dbFacts = newFacts, dbFinished = done }

studyFacts :: Int -> StudyDate -> [Fact] -> [Fact] -> IO ([Fact], [Fact])
studyFacts 0 _ ds fs = putStrLn "Nothing more for today B)" >> return (fs, ds)
studyFacts _ _ _ [] = putStrLn "There are no facts in your database" >> return ([], [])
studyFacts n today ds (f:fs)
  | factStudyDate f > today = putStrLn "Nothing more for today B)" >> return (ds, (f:fs))
  | otherwise = do
      putStrLn (factQuestion f)
      c <- getChar
      case c of _ -> return ()
      putStrLn (factAnswer f)
      confidence <- getConfidence
      let nextConfidence = incrementConfidence confidence (factConfidence f)
      if nextConfidence == Unforgettable && factConfidence f == Unforgettable
        then studyFacts (n - 1) today (f:ds) fs
        else let f' = f { factConfidence = nextConfidence, factStudyDate = nextRepitition today nextConfidence } in studyFacts (n - 1) today ds (fs ++ [f'])

incrementConfidence Unforgettable Unforgettable = Unforgettable
incrementConfidence newC oldC | newC >= oldC = succ oldC
                              | otherwise    = newC

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
  t <- getZonedTime
  let (y, m, d) = toGregorian $ localDay $ zonedTimeToLocalTime t
  return $ StudyDate y m d

incDate (StudyDate y m d) days = let (y', m', d') = toGregorian $ addDays days $ fromGregorian y m d in StudyDate y' m' d'
