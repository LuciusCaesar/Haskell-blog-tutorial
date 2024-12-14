module HsBlog.Html.Internal where

import GHC.Natural (Natural)

newtype Html = Html String

newtype Structure = Structure String

type Title = String

instance Semigroup Structure where
  (<>) (Structure x) (Structure y) = Structure (x <> y)

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

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

empty_ :: Structure
empty_ = Structure ""

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

instance Monoid Structure where
  mempty = empty_