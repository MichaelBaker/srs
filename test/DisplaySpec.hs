import Test.Hspec (hspec, describe, it, shouldBe)
import Display    (showColumns)

main = hspec $ do
  describe "showColumns" $ do
    it "converts a pair of strings into a single string with the first element representing the first column and the first element representing the second column" $ do
      testCases 3 1 [
          ((["a"], ["a"]), "a  a")
        , ((["ab", "a"], ["a"]), "ab a\na   ")
        , ((["ab", "a"], ["a", "b"]), "ab a\na  b")
        , ((["a"], ["a", "b"]), "a  a\n   b")
        ]

    it "wraps column one" $ do
      testCases 4 1 [
          ((["aaaaa"], ["b"]), "aaaab\na    ")
        , ((["aaaaaaaaa"], ["b"]), "aaaab\naaaa \na    ")
        ]

    it "wraps column two" $ do
      testCases 1 4 [
          ((["a"], ["bbbbb"]), "abbbb\n b   ")
        ]

    it "removes the whitespace from the beginning of each line" $ do
      testCases 4 1 [
          ((["aa  a"], ["b"]), "aa  b\na    ")
        ]

    it "remove all newlines that might screw everything up" $ do
      testCases 5 1 [
          ((["a\naa"], ["b"]), "a aa b")
        ]

testCases wrapOne wrapTwo cs = mapM_ runTest cs
  where runTest (input, expected) = showColumns wrapOne wrapTwo input `shouldBe` expected
