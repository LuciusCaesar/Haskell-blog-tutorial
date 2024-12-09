module Html
  ( Html,
    Title,
    Structure,
    html_,
    p_,
    h1_,
    append_,
    render,
  )
where

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
            (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 <> s2)

getStructureString :: Structure -> String
getStructureString (Structure s) = s

render :: Html -> String
render (Html s) = s

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
