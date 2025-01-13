module MarkupParsingSpec where

import BlogGenerator.Markup
import Test.Hspec

spec :: Spec
spec =
  describe
    "Markup parsing tests"
    $ it
      "Empty string parses to empty doc"
      ( shouldBe
          (parse "")
          []
      )
