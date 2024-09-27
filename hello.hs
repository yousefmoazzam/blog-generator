main = putStrLn myhtml

myhtml = makeHtml "My page title" "Some header" "Hello, world!"

makeHtml :: String -> String -> String -> String
makeHtml title header paragraph = html_ (el "head" (el "title" title) <> el "body" (h1_ header <> p_ paragraph))

html_ :: String -> String
html_ = el "html"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render (Html str) = str

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

-- Types
newtype Html = Html String

newtype Structure = Structure String

type Title = String
