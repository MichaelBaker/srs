import Options.Applicative   (subparser, command, info, progDesc, execParser, helper, idm)
import Control.Applicative   ((<$>), (<*>))

import qualified Srs

data Command = SrsCommand Srs.SrsCommand deriving (Show)

options = subparser (command "srs" (info (helper <*> (SrsCommand <$> Srs.options)) $ progDesc "Spaced Repetition Software"))

main = do
  parsedCommand  <- execParser (info (helper <*> options) idm)

  case parsedCommand of
    SrsCommand subcommand -> Srs.runCommand subcommand
