module HsBlog.Html.Internal where

import GHC.Natural (Natural)

---- DATA --------

newtype Html = Html String

newtype Head = Head String

newtype Structure = Structure String

type Title = String

instance Semigroup Structure where
  (<>) (Structure x) (Structure y) = Structure (x <> y)

instance Monoid Structure where
  mempty = empty_

instance Semigroup Head where
  (<>) (Head x) (Head y) = Head (x <> y)

instance Monoid Head where
  mempty = Head ""

newtype Content = Content String

----- FUNCTIONS ----------

html_ :: Head -> Structure -> Html
html_ (Head h) body = 
  Html
  $ el "html" 
    $ el "head" h
      <> el "body" (getStructureString body)
      
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttrs :: String -> String -> String -> String
elAttrs tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

title_ :: Title -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ path = Head $ "<link rel = \"stylesheet\" type = \"text/css\" href = \"" <> escape path <> "\">"

meta_ :: String -> String -> Head
meta_ name content = Head $ "<meta name = \"" <> escape name <> "\" content = \"" <> escape content <> "\">"

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