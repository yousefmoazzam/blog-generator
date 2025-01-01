import Convert
import System.Directory
import System.Environment

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      -- Open input, process contents, write to output
      [first, second] -> processInputWriteOutput first second

processInputWriteOutput :: FilePath -> FilePath -> IO ()
processInputWriteOutput input output =
  readFile input >>= \contents ->
    pure
      (Convert.process input contents)
      >>= \processed -> writeFile output processed
