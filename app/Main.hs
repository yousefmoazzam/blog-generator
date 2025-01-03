module Main where

import qualified BlogGenerator
import Data.Maybe (fromMaybe)
import Options.Applicative

main :: IO ()
main = BlogGenerator.main

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

inp :: Parser FilePath
inp =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input file"
    )

out :: Parser FilePath
out =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file"
    )

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
  where
    parser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file"
        )

pSingleInput :: Parser SingleInput
pSingleInput = fmap (fromMaybe Stdin) (optional pInputFile)

pSingleOutput :: Parser SingleOutput
pSingleOutput = fmap (fromMaybe Stdout) (optional pOutputFile)

pConvertSingle :: Parser Options
pConvertSingle = liftA2 ConvertSingle pInputFile pOutputFile

pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "DIR"
        <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "DIR"
        <> help "Output directory"
    )

pConvertDir :: Parser Options
pConvertDir = liftA2 ConvertDir pInputDir pOutputDir
