import GeniusYield.Server.Options
import Options.Applicative

main ∷ IO ()
main = runCommand =<< execParser opts
 where
  opts =
    info
      (parseCommand <**> helper)
      ( fullDesc
          <> progDesc "GeniusYield DEX helpful operations"
          <> header "GeniusYield DEX"
      )