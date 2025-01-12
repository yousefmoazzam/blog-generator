module BlogGenerator.Env (Env (..)) where

data Env
  = Env
  { eBlogName :: String,
    eStylesheetPath :: FilePath
  }
  deriving (Show)
