module HsBlog.Html.Internal where

import GHC.Natural (Natural)

---- DATA --------

newtype Html = Html String

newtype Structure = Structure String

type Title = String

instance Semigroup Structure where
  (<>) (Structure x) (Structure y) = Structure (x <> y)

instance Monoid Structure where
  mempty = empty_

newtype Content = Content String

----- FUNCTIONS ----------

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

elAttrs :: String -> String -> String -> String
elAttrs tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

h1_ :: Content -> Structure
h1_ = Structure . el "h1" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: Content -> Structure
code_ = Structure . el "pre" . getContentString

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
   in concatMap escapeChar

------ CONTENT ---------

txt_ :: String -> Content
txt_ = Content . escape

img_ :: FilePath -> Content
img_ path = Content $ "<img src = \"" <> escape path <> "\" />"

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttrs
      "a"
      ("href = \"" <> escape path <> "\"")
      (getContentString content)

b_ :: Content -> Content
b_ = Content . el "b" . getContentString

i_ :: Content -> Content
i_ = Content . el "i" . getContentString

getContentString :: Content -> String
getContentString (Content s) = s