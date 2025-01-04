module Main where

import qualified BlogGenerator
import OptParse
import Options.Applicative (execParser)
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openFile, stdin, stdout)

main :: IO ()
main =
  execParser OptParse.opts >>= \args ->
    case args of
      OptParse.ConvertDir _ _ ->
        putStrLn "todo"
      OptParse.ConvertSingle input output ->
        ( case input of
            OptParse.Stdin ->
              pure System.IO.stdin
            OptParse.InputFile inPath ->
              openFile inPath ReadMode
        )
          >>= \inHandle ->
            ( case output of
                OptParse.Stdout ->
                  pure System.IO.stdout
                OptParse.OutputFile outPath ->
                  doesFileExist outPath >>= \doesExist ->
                    ( if doesExist
                        then
                          putStrLn "Output file already exists, exiting"
                            *> exitFailure
                        else openFile outPath WriteMode
                    )
            )
              >>= \outHandle ->
                BlogGenerator.convertSingle inFilename inHandle outHandle
                  *> hClose inHandle
                  *> hClose outHandle
        where
          inFilename = case input of
            OptParse.Stdin -> "stdin"
            OptParse.InputFile path -> path
