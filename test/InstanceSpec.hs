{-# LANGUAGE MultiParamTypeClasses #-}

module InstanceSpec where

import           Data.Set             (Set)
import qualified Data.Set             as S
import           GHC.TypeLits
import           InstanceVia
import           Test.Hspec
import           TransitiveAnns.Types


test :: forall (comp :: Symbol) (name :: Symbol) x. AddAnnotation 'Remote comp name x => Int
test = 5

class Hello where
  hello :: Int

instance Hello where
  hello = test @"brig" @"hello"

ahello :: Set Annotation
ahello = annotated hello

ahelloVia :: Set Annotation
ahelloVia = annotated helloVia

spec :: Spec
spec = do
  xit "should propagate via instances" $ do
    ahello `shouldBe` S.fromList
      [ Annotation Remote "brig" "hello"
      ]

  it "should propagate via instances across modules" $ do
    ahelloVia `shouldBe` S.fromList
      [ Annotation Remote "brig" "helloVia"
      ]

