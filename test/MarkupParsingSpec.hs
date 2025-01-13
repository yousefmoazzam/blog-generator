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
