module Convert where

import qualified Html
import qualified Markup

process :: Html.Title -> String -> String
process title content = Html.render $ convert title $ Markup.parse content

convert :: String -> Markup.Document -> Html.Html
convert title doc = Html.html_ title (foldMap convertStructure doc)

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt
    Markup.Paragraph p ->
      Html.p_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list
    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)
