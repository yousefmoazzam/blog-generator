module BlogGenerator.Convert where

import qualified BlogGenerator.Html as Html
import qualified BlogGenerator.Markup as Markup

-- | Convert markup represented by `Markup.Document` to HTML represented by `Html.Html`
convert :: String -> Markup.Document -> Html.Html
convert title doc = Html.html_ title (foldMap convertStructure doc)

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n (Html.txt_ txt)
    Markup.Paragraph p ->
      Html.p_ (Html.txt_ p)
    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)
