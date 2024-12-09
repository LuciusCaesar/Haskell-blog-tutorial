main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
        (h1_ "Heading")
        ( append_
            (p_ "Paragraph #1")
            (p_ "Paragraph #2")
        )
    )

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el
            "head"
            (el "title" title)
            <> el "body" (getStructureString content)
        )
    )

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

p_ :: String -> Structure
p_ = Structure . el "p"

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 <> s2)

getStructureString :: Structure -> String
getStructureString (Structure s) = s

render :: Html -> String
render (Html s) = s