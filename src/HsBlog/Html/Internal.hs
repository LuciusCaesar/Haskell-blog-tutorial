-- | This module contains the internal representation of the HTML DSL.
module HsBlog.Html.Internal where

import GHC.Natural (Natural)

-- * Data types

-- | The HTML type represents an HTML document.
newtype Html = Html String

-- | The Head type represents the @\<head\>@ section of an HTML document.
newtype Head = Head String

-- | The Structure type represents the @\<body\>@ section of an HTML document.
newtype Structure = Structure String

-- | The Content type represents the content of an HTML tag.
newtype Content = Content String

-- | The Title type represents the title of an HTML document.
type Title = String


-- | The 'Semigroup' instance for 'Html' concatenates the HTML strings.
instance Semigroup Structure where
  (<>) (Structure x) (Structure y) = Structure (x <> y)

-- | The 'Monoid' instance for 'Html' uses 'mempty' as the empty HTML document.
instance Monoid Structure where
  mempty = empty_

-- | The 'Semigroup' instance for 'Head' concatenates the head strings.
instance Semigroup Head where
  (<>) (Head x) (Head y) = Head (x <> y)

-- | The 'Monoid' instance for 'Head' uses 'mempty' as the empty head section.
instance Monoid Head where
  mempty = Head ""

-- * Functions

-- ** HTML EDSL

-- | Create an HTML document.
html_ :: Head -> Structure -> Html
html_ (Head h) body = 
  Html
  $ el "html" 
    $ el "head" h
      <> el "body" (getStructureString body)

-- ** Utils 

-- | Create an an html tag with with a given tag name and content.     
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- | Create an an html tag with with a given tag name, attributes and content.
elAttrs :: String -> String -> String -> String
elAttrs tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

-- ** @\<head\>@ section

-- | Create a title tag.
title_ :: Title -> Head
title_ = Head . el "title" . escape

-- | Create a stylesheet link tag.
stylesheet_ :: FilePath -> Head
stylesheet_ path = Head $ "<link rel = \"stylesheet\" type = \"text/css\" href = \"" <> escape path <> "\">"

-- | Create a meta tag.
meta_ :: String -> String -> Head
meta_ name content = Head $ "<meta name = \"" <> escape name <> "\" content = \"" <> escape content <> "\">"


-- ** @\<body\>@ section

-- | Create an @\<h1\>@ tag.
h1_ :: Content -> Structure
h1_ = Structure . el "h1" . getContentString

-- | Create an @\<h \>@ tag with a given level.
h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

-- | Create a @\<p\>@ tag.
p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

-- | Create an @\<ul\>@ tag.
ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

-- | Create an @\<ol\>@ tag.
ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

-- | Create a @\<pre\>@ tag.
code_ :: Content -> Structure
code_ = Structure . el "pre" . getContentString

-- | Create an empty structure.
empty_ :: Structure
empty_ = Structure ""

-- | Extract the content from a Structure as a String
getStructureString :: Structure -> String
getStructureString (Structure s) = s

-- ** Content inside Sections

-- | Create a text content.
txt_ :: String -> Content
txt_ = Content . escape

-- | Create an image content.
img_ :: FilePath -> Content
img_ path = Content $ "<img src = \"" <> escape path <> "\" />"

-- | Create a link content.
link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttrs
      "a"
      ("href = \"" <> escape path <> "\"")
      (getContentString content)

-- | Create a bold content.
b_ :: Content -> Content
b_ = Content . el "b" . getContentString

-- | Create an italic content.
i_ :: Content -> Content
i_ = Content . el "i" . getContentString

-- | Extract the content from a Content as a String
getContentString :: Content -> String
getContentString (Content s) = s

-- ** Render HTML to String

-- | Render an HTML document to a string.
render :: Html -> String
render (Html s) = s

-- ** Utils

-- | Escape special characters in a string.
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