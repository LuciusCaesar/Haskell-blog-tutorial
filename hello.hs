import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title <html> awesome"
    ( append_
        (h1_ "Heading <br /> test")
        ( append_
            (p_ "Paragraph \\ plip #1")
            (p_ "Paragraph // plop #2")
        )
    )
