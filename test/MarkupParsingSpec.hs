module MarkupParsingSpec where

import BlogGenerator.Markup
import Test.Hspec

spec :: Spec
spec =
  describe
    "Markup parsing tests"
    $ do
      it
        "Empty string parses to empty doc"
        ( shouldBe
            (parse "")
            []
        )

      it
        "Paragraph markup parses to paragraph variant"
        ( let str = "Hello world!"
           in shouldBe
                (parse str)
                [Paragraph str]
        )

      it
        "Heading markup parses to heading variant with size 1"
        ( let str = "Some Heading"
           in shouldBe
                (parse $ "* " ++ str)
                [Heading 1 str]
        )

      it
        "Code markup parses to code block variant"
        ( let str = "main = putStrLn \"hello world!\""
           in shouldBe
                (parse $ "> " ++ str)
                [CodeBlock [str]]
        )
