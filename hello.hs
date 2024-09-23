main = putStrLn myhtml

myhtml = makeHtml "My page title" "My page content"

makeHtml title content = html_ (head_ (title_ title) <> body_ content)

html_ content = "<html>" <> content <> "</html>"

body_ content = "<body>" <> content <> "</body>"

head_ content = "<head>" <> content <> "</head>"

title_ content = "<title>" <> content <> "</title>"
