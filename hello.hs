main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My page title"
    ( Structure
        ( el
            "body"
            ( getStructureString
                ( append_
                    (h1_ "Some header")
                    (append_ (p_ "Paragraph 1: Hello, world!") (p_ "Paragraph 2: Some other thing"))
                )
            )
        )
    )

html_ :: Title -> Structure -> Html
html_ title bodyStructure =
  Html
    ( el
        "html"
        ( getStructureString
            ( append_
                (Structure (el "head" (el "title" title)))
                bodyStructure
            )
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render (Html str) = str

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

getStructureString :: Structure -> String
getStructureString (Structure str) = str

-- Types
newtype Html = Html String

newtype Structure = Structure String

type Title = String
