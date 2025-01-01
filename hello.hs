import Convert
import System.Directory
import System.Environment

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      -- Open input, process contents, write to output
      [first, second] ->
        readFile first >>= \contents ->
          pure
            (Convert.process first contents)
            >>= \processed -> writeFile second processed
