module BlogGenerator (convertSingle) where

import BlogGenerator.Convert as Convert
import BlogGenerator.Html as Html
import BlogGenerator.Markup as Markup
import System.IO (Handle, hGetContents, hPutStrLn)

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output =
  hGetContents input >>= \contents ->
    hPutStrLn output (process title contents)

-- | Convert markup string to HTML string
process :: Html.Title -> String -> String
process title content = Html.render $ Convert.convert title $ Markup.parse content
