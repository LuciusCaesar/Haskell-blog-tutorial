import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title <html> awesome"
    ( h1_ "Heading <br /> test"
        <> ul_
          [ p_ "item 1",
            p_ "item 2",
            p_ "item 3"
          ]
    )
