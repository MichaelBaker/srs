import Prelude hiding        (readFile)
import Options.Applicative   (subparser, long, short, optional, help, strOption, command, info, progDesc, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>), pure)
import Data.Monoid           ((<>))
import System.Directory      (doesFileExist, getHomeDirectory)
import System.FilePath.Posix (joinPath)
import System.IO             (BufferMode (..), hSetBuffering, stdout, stdin)
import System.IO.Strict      (readFile)
import Safe                  (readMay)
import Database              (Database(), srs, list, emptyDatabase)

import qualified Srs
import qualified List

data Command = Command { dbPath :: Maybe String, subCommand :: SubCommand } 

data SubCommand = SrsCommand  Srs.SrsCommand
                | ListCommand List.ListCommand
                | CreateDatabase
                deriving (Show)

options = Command <$> optional (strOption (long "database" <> short 'd' <> help "An optional path to the database file"))
                  <*> subparser
                    (  command "srs"    (info (helper <*> (SrsCommand <$> Srs.options)) $ progDesc "Spaced Repetition Software")
                    <> command "list"   (info (helper <*> (ListCommand <$> List.options)) $ progDesc "List tracker")
                    <> command "create" (info (helper <*> (pure CreateDatabase))$ progDesc "Creates an empty zgy database")
                    )

main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

  parsedCommand  <- execParser (info (helper <*> options) idm)

  databasePath <- case dbPath parsedCommand of
    Nothing  -> getHomeDirectory >>= (\home -> return $ joinPath [home, ".srs-database"])
    Just dir -> return dir

  run databasePath (subCommand parsedCommand)


run path CreateDatabase = do
  dbExists <- doesFileExist path
  if dbExists
    then putStrLn ("A file already exists at '" ++ path ++ "'.")
    else writeFile path $ show emptyDatabase

run path c = do
  dbExists <- doesFileExist path

  if not dbExists
    then putStrLn ("No database exists at '" ++ path ++ "'. You can create it with the 'create' subcommand.")
    else do
      maybeDatabase <- readFile path >>= (return . readMay) :: IO(Maybe Database)
      case maybeDatabase of
        Nothing -> putStrLn ("The database at '" ++ path ++ "' is invalid.")
        Just database -> do
          newDb <- case c of
            SrsCommand subcommand -> do
              newSrs <- Srs.run (srs database) subcommand
              return database { srs = newSrs }
            ListCommand subcommand -> do
              newList <- List.run (list database) subcommand
              return database { list = newList }
            CreateDatabase -> return database
          writeFile path $ show newDb
