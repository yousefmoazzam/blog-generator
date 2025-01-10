module BlogGenerator.Directory (buildIndex) where

import BlogGenerator.Html as Html
import BlogGenerator.Markup as Markup
import Control.Exception (SomeException (..), catch, displayException)
import Data.Maybe (listToMaybe)

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

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList func vals =
  sequenceA (map process vals)
  where
    process input =
      catch
        ( func input >>= \output ->
            pure (input, Right output)
        )
        (\e -> pure (input, Left $ displayException (e :: SomeException)))

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures vals =
  mconcat (map process vals)
  where
    process (first, second) =
      case second of
        Left e ->
          putStrLn ("IO error: " ++ e)
            *> pure []
        Right inner ->
          pure [(first, inner)]
