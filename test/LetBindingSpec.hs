{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module LetBindingSpec where

import qualified Data.Set             as S
import           GHC.TypeLits
import           Test.Hspec
import           TransitiveAnns.Types

fedClient
    :: forall (api :: Symbol) (name :: Symbol) x
     . AddAnnotation 'Remote api name x
    => Bool
    -> Int
fedClient _ = 5

{-# ANN notifyUserDeleted "HLint: ignore Evaluate" #-}
notifyUserDeleted :: String -> ()
notifyUserDeleted str = do
  let b = null str
  const () $
    fedClient @"brig" @"on-user-deleted-connections" $
      b

spec :: Spec
spec = do
  it "should correctly expand complicated function body" $ do
    annotated notifyUserDeleted `shouldBe` S.fromList
      [ Annotation Remote "brig" "on-user-deleted-connections"
      ]

