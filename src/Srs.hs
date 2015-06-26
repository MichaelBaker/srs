module Srs where

import Prelude hiding        (getLine)
import Data.Time             (getZonedTime, localDay, zonedTimeToLocalTime, toGregorian, fromGregorian, addDays)
import Safe                  (readMay)
import Data.List             (sortBy, intercalate)
import Display               (showColumns)
import Control.Applicative   ((<$>), (<*>), pure)
import Options.Applicative   (Parser(), subparser, command, info, progDesc, argument, metavar, auto, str, helper)
import Data.Monoid           ((<>))
import SrsDatabase           (SrsDatabase(..), Fact(..), StudyDate(..))
import System.IO             (getLine)

data SrsCommand = Add    { addConfidence :: Int, addQuestion :: String, addAnswer :: String }
                | Remove { removeFactId :: Int }
                | Study
                | List
                deriving (Show)

addOptions    = Add <$> argument auto (metavar "CONFIDENCE") <*> argument str (metavar "QUESTION") <*> argument str (metavar "ANSWER")
removeOptions = Remove <$> argument auto (metavar "FACTID")
listOptions   = pure List
studyOptions  = pure Study

options :: Parser SrsCommand
options = subparser
  (  command "add"    (info (helper <*> addOptions)    $ progDesc "Adds a fact to study")
  <> command "remove" (info (helper <*> removeOptions) $ progDesc "Removes a fact from the database")
  <> command "list"   (info (helper <*> listOptions)   $ progDesc "List all facts")
  <> command "study"  (info (helper <*> studyOptions)  $ progDesc "Study todays facts")
  )

runCommand db subcommand = do
  newDb <- run db subcommand
  return $ sortFacts newDb

run :: SrsDatabase -> SrsCommand -> IO SrsDatabase
run db List = do
  let factData f = ([show (factId f) ++ "  " ++ show (factConfidence f), timeString (factStudyDate f)], ["|Q| " ++ factQuestion f, "|A| " ++ factAnswer f])
      timeString (StudyDate y m d) = concat [show y, "-", show m, "-", show d]
  putStrLn $ intercalate "\n\n" $ map (showColumns 19 60 . factData) (dbFacts db)
  return db
run db (Add c q a) = do
  if c >= minConfidence && c <= maxConfidence
     then do
      studyDate <- todaysStudyDate
      let newFact = Fact c studyDate (dbNextId db) q a
      return db { dbNextId = dbNextId db + 1, dbFacts = dbFacts db ++ [newFact] }
    else do
      putStrLn (show c ++ " is not a valid confidence level. Please choose a positive integer between 1 and 5")
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
studyFacts _ _ ds [] = putStrLn "There are no facts in your database" >> return ([], ds)
studyFacts n today ds (f:fs)
  | factStudyDate f > today = putStrLn "Nothing more for today B)" >> return ((f:fs), ds)
  | otherwise = do
      putStrLn ""
      putStr (factQuestion f)
      c <- getLine
      case c of _ -> return ()
      putStrLn ""
      putStrLn (factAnswer f)
      confidence <- getConfidence
      let nextConfidence = incrementConfidence confidence (factConfidence f)
      if nextConfidence == maxConfidence && factConfidence f == maxConfidence
        then studyFacts (n - 1) today (f:ds) fs
        else let f' = f { factConfidence = nextConfidence, factStudyDate = nextRepitition today nextConfidence } in studyFacts (n - 1) today ds (fs ++ [f'])

incrementConfidence new old = if new > old then old + 1 else new

nextRepitition today n = incDate today (2 ^ n)

getConfidence :: IO Int
getConfidence = do
  putStrLn ""
  putStr $ "How well did you know that?(" ++ show minConfidence ++ "-" ++ show maxConfidence ++ "): "
  c <- getLine
  case readMay c of
   Nothing -> putStrLn (show c ++ " is not a valid confidence level. Please enter an integer between " ++ show minConfidence ++ " and " ++ show maxConfidence) >> getConfidence
   Just a  -> if a >= minConfidence && a <= maxConfidence
                then return a
                else putStrLn (show a ++ " is not a valid confidence level. Please enter an integer between " ++ show minConfidence ++ " and " ++ show maxConfidence) >> getConfidence

todaysStudyDate = do
  t <- getZonedTime
  let (y, m, d) = toGregorian $ localDay $ zonedTimeToLocalTime t
  return $ StudyDate y m d

incDate (StudyDate y m d) days = let (y', m', d') = toGregorian $ addDays days $ fromGregorian y m d in StudyDate y' m' d'

sortFacts db = db { dbFacts = newFacts }
  where newFacts = sortBy (\a b -> compare (chronological (factStudyDate a) (factId a)) (chronological (factStudyDate b) (factId b))) $ dbFacts db
        chronological (StudyDate y m d) n = (y, m, d, n)

maxConfidence = 5
minConfidence = 1

wordWrap maxLen s = toLines "" (words s)
  where toLines l []     = [l]
        toLines l (w:ws) = if length l + length w > maxLen
                             then l : toLines w ws
                             else toLines (unwords [l, w]) ws
