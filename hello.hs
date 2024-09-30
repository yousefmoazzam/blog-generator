import Html

main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My page title"
    ( append_
        (h1_ "Some header")
        (append_ (p_ "Paragraph 1: Hello, world!") (p_ "Paragraph 2: Some other thing"))
    )
