module HsBlog.Convert where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import HsBlog.Env (Env(..))

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc =
  let
    h =
      Html.title_ (eBlogName env <> " - " <> title)
        <> Html.stylesheet_ (eStylesheetPath env)
    article =
      foldMap convertStructure doc
    websiteTitle =
      Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
    body =
      websiteTitle <> article
  in
    Html.html_ h body

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt
    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.CodeBlock list ->
      Html.code_ $ Html.txt_ (unlines list)

