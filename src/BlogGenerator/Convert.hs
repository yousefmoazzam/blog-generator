module BlogGenerator.Convert where

import qualified BlogGenerator.Html as Html
import qualified BlogGenerator.Markup as Markup
import Data.Maybe (listToMaybe)

process :: Html.Title -> String -> String
process title content = Html.render $ convert title $ Markup.parse content

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

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex input =
  let title = "Index"
      top = Html.h_ 1 (Html.txt_ title)
      summariseDoc structures =
        case listToMaybe structures of
          Nothing -> Html.h_ 2 (Html.txt_ "")
          Just structure ->
            case structure of
              Markup.Heading _ content -> Html.h_ 2 (Html.txt_ content)
              Markup.Paragraph content -> Html.h_ 2 (Html.txt_ $ (head . lines) content)
              _ -> Html.h_ 2 (Html.txt_ "")
      createLinkAndSummary (path, doc) =
        Html.p_ (Html.link_ path (Html.txt_ "")) <> summariseDoc doc
   in Html.html_ title (top <> foldMap createLinkAndSummary input)
