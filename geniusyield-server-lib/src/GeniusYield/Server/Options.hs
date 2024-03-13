module GeniusYield.Server.Options where

-- TODO: Explicit export list.

import GeniusYield.Server.Run (runServer)
import Options.Applicative
import RIO

newtype Command = Serve ServeCommand

newtype ServeCommand = ServeCommand (Maybe FilePath)

parseCommand ∷ Parser Command
parseCommand =
  subparser $
    mconcat
      [ command
          "serve"
          ( info (Serve <$> parseServeCommand <**> helper) $
              progDesc "Serve endpoints"
          )
      ]

parseServeCommand ∷ Parser ServeCommand
parseServeCommand =
  ServeCommand
    <$> optional
      ( strOption
          ( long "config"
              <> metavar "CONFIG"
              <> short 'c'
              <> help "Path of optional configuration file. If not provided, \"SERVER_CONFIG\" environment variable is used."
          )
      )

runCommand ∷ Command → IO ()
runCommand (Serve serveCommand) = runServeCommand serveCommand

runServeCommand ∷ ServeCommand → IO ()
runServeCommand (ServeCommand mcfp) = runServer mcfp
