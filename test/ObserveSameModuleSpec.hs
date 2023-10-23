module ObserveSameModuleSpec where

import qualified Data.Set             as S
import           ObserveSameModule
import           Test.Hspec
import           TransitiveAnns.Types

spec :: Spec
spec = describe "observe annotationsVal in same module" $ do
  it "attached via ANN" $ observeRef `shouldBe` S.fromList [Annotation Local "ref" "ref"]
  it "attached via AddAnnotation" $ observeAref `shouldBe` S.fromList [Annotation Local "aref" "aref"]
  it "attached indirectly via AddAnnotation" $ observeAref2 `shouldBe` S.fromList [Annotation Local "aref" "aref"]
