module BlogGenerator.Env (Env (..), defaultEnv) where

data Env
  = Env
  { eBlogName :: String,
    eStylesheetPath :: FilePath
  }
  deriving (Show)

defaultEnv :: Env
defaultEnv = Env "Test Blog" "style.css"
