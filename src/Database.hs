module Database where

data Database   = Database { dbNextId :: Int, dbFacts :: [Fact], dbFinished :: [Fact] } deriving (Read, Show)
data Fact       = Fact { factConfidence :: Int, factStudyDate :: StudyDate, factId :: Int, factQuestion :: String, factAnswer :: String } deriving (Read, Show)
data StudyDate  = StudyDate { studyYear :: Integer, studyMonth :: Int, studyDay :: Int } deriving (Read, Show, Eq, Ord)

emptyDatabase = Database { dbNextId = 0, dbFacts = [], dbFinished = [] }
