import Convert
import System.Directory
import System.Environment

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      -- Input from stdin, process contents, write to stdout
      [] ->
        getContents >>= \contents ->
          pure (Convert.process "stdin" contents) >>= \processed ->
            putStrLn processed
      -- Open input, process contents, write to output
      [first, second] ->
        doesFileExist second >>= \doesExist ->
          case doesExist of
            False -> processInputWriteOutput first second
            True ->
              confirm >>= \confirmed ->
                case confirmed of
                  False -> pure ()
                  True -> processInputWriteOutput first second
      _ -> putStrLn programUsageText

processInputWriteOutput :: FilePath -> FilePath -> IO ()
processInputWriteOutput input output =
  readFile input >>= \contents ->
    pure
      (Convert.process input contents)
      >>= \processed -> writeFile output processed

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine
    >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n"
            *> confirm

programUsageText :: String
programUsageText =
  "Program usage\n\
  \<no args>: read input from stdin, write output to stdout\n\
  \<input-filepath> <output-filepath>: read input from first arg, write output to second arg"
