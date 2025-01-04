module OptParse where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput Bool
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

pReplace :: Parser Bool
pReplace =
  switch
    ( long "replace"
        <> short 'r'
        <> help "Silently overwrite output if it exists"
    )

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
pConvertSingle = liftA3 ConvertSingle pInputFile pOutputFile pReplace

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

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

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo =
  info
    (helper <*> pConvertDir)
    (progDesc "Convert a directory of markup source files to html")

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand =
  command "convert-dir" pConvertDirInfo

pOptions :: Parser Options
pOptions =
  subparser (pConvertSingleCommand <> pConvertDirCommand)

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "blog-generator - a static blog generator"
        <> progDesc "Convert markup files or directories to html"
    )
