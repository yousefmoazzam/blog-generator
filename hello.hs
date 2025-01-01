import Convert
import System.Directory
import System.Environment

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
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
