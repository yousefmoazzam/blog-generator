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
        "Multi-line string with only one newline char between parses to single paragraph"
        ( let lineOne = "Some text in line one"
              lineTwo = "and more text in line two."
           in shouldBe
                (parse $ lineOne ++ "\n" ++ lineTwo)
                [Paragraph $ lineOne ++ " " ++ lineTwo]
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

      it
        "Unordered list markup parses to unordered list variant"
        ( let itemOne = "Item 1"
              itemTwo = "Item 2"
           in shouldBe
                (parse $ "- " ++ itemOne ++ "\n- " ++ itemTwo)
                [UnorderedList [itemOne, itemTwo]]
        )

      it
        "Ordered list markup parses to ordered list variant"
        ( let itemOne = "Item 1"
              itemTwo = "Item 2"
           in shouldBe
                (parse $ "# " ++ itemOne ++ "\n# " ++ itemTwo)
                [OrderedList [itemOne, itemTwo]]
        )
