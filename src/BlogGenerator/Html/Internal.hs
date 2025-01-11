module BlogGenerator.Html.Internal where

import GHC.Natural (Natural)

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

instance Semigroup Structure where
  (Structure a) <> (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = empty_

type Title = String

html_ :: Title -> Structure -> Html
html_ title bodyStructure =
  Html
    ( el
        "html"
        ( el "head" $
            getStructureString (title_ (escape title))
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

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> Content -> Structure
h_ num = Structure . el ("h" <> show num) . getContentString

title_ :: String -> Structure
title_ content = Structure $ el "title" content

meta_ :: [(String, String)] -> Structure
meta_ pairs =
  Structure $
    elAttr
      "meta"
      (unwords attrs)
      (getStructureString empty_)
  where
    attrs = map generateAttrStr pairs
    generateAttrStr (attr, val) =
      attr <> "=" <> "\"" <> escape val <> "\""

empty_ :: Structure
empty_ = Structure ""

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

txt_ :: String -> Content
txt_ = Content . escape

stylesheet_ :: FilePath -> Content
stylesheet_ path =
  Content $
    elAttr
      "link"
      ("href=\"" <> escape path <> "\"" <> " " <> fixedAttrs)
      ""
  where
    fixedAttrs = unwords ["rel=\"stylesheet\"", "type=\"text/css\""]

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

instance Semigroup Content where
  (<>) c1 c2 =
    Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
  mempty = Content ""

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getContentString :: Content -> String
getContentString (Content str) = str
