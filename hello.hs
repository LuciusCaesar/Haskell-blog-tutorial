main = putStrLn myhtml

myhtml = makeHtml "Hello title" "Hello, world!"

makeHtml title body = html_ $ head_ $ title_ title <> body_ body

html_ s = "<html>" <> s <> "</html>"

body_ s = "<body>" <> s <> "</body>"

head_ s = "<head>" <> s <> "</head>"

title_ s = "<title>" <> s <> "</title>"
