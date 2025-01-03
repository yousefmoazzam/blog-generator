module BlogGenerator (main, convertSingle) where

import BlogGenerator.Convert as Convert
import BlogGenerator.Html as Html
import System.Directory
import System.Environment
import System.IO (Handle, hGetContents, hPutStrLn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Input from stdin, process contents, write to stdout
    [] -> do
      contents <- getContents
      let processed = Convert.process "stdin" contents
      putStrLn processed
    -- Open input, process contents, write to output
    [first, second] -> do
      doesExist <- doesFileExist second
      case doesExist of
        False -> processInputWriteOutput first second
        True -> do
          confirmed <- confirm
          case confirmed of
            False -> pure ()
            True -> processInputWriteOutput first second
    _ -> putStrLn programUsageText

processInputWriteOutput :: FilePath -> FilePath -> IO ()
processInputWriteOutput input output = do
  contents <- readFile input
  let processed = Convert.process input contents
  writeFile output processed

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. use y or n" *> confirm

programUsageText :: String
programUsageText =
  "Program usage\n\
  \<no args>: read input from stdin, write output to stdout\n\
  \<input-filepath> <output-filepath>: read input from first arg, write output to second arg"

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output =
  hGetContents input >>= \contents ->
    hPutStrLn output (Convert.process title contents)
