import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word =
  let (Just result) = findWord gwc word
      string = map cell2char result
  in string `shouldBe` word

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a nl" $ do
      (formatGrid (gridWithCoords ["asd", "bca", "qwe"])) `shouldBe` "asd\nbca\nqwe\n"

  describe "findWord" $ do
    it "Should find a word if it exists on the grid" $ do
      testFindWord "HASKELL"
      testFindWord "PERL"
    it "Should not find words that don't exist in the grid" $ do
      findWord gwc "HAMSTER" `shouldBe` Nothing
      
  describe "findWords" $ do
    it "Should find every word on a list if they exist on the grid" $ do
      let found = findWords gwc languages
          asString = map (map cell2char) found
      asString `shouldBe` languages
      -- findWords gwc languages `shouldBe` languages
    it "Should not find words that don't exist in a grid" $ do
      findWords gwc ["SOMETHING", "WONDERFUL"] `shouldBe` []
