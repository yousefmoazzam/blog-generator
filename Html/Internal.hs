module Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title bodyStructure =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString bodyStructure)
        )
    )

ul_ :: [Structure] -> Structure
ul_ items =
  let wrappedItems = map li_ items
   in Structure (el "ul" (concatMap getStructureString wrappedItems))

ol_ :: [Structure] -> Structure
ol_ items =
  let wrappedItems = map li_ items
   in Structure (el "ol" (concatMap getStructureString wrappedItems))

li_ :: Structure -> Structure
li_ = Structure . el "li" . getStructureString

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

render :: Html -> String
render (Html str) = str

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concat . map escapeChar
