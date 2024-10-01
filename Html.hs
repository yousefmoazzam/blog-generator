module Html
  ( Html,
    Title,
    Structure,
    html_,
    h1_,
    p_,
    append_,
    render,
  )
where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title bodyStructure =
  Html
    ( el
        "html"
        ( el "head" (el "title" title)
            <> el "body" (getStructureString bodyStructure)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

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
