{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

--import           Control.Monad
--import           Control.Exception
import           Data.Data         (Data)
import qualified Data.IxSet.Typed  as IX 
import Data.IxSetPk.Typed as IXP
--import           Data.Maybe
import qualified Data.Set          as S
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck
--import Data.List (foldl')

import System.IO.Unsafe

testSpec' :: TestName -> Spec -> TestTree
testSpec' x = unsafePerformIO . testSpec x
  

data Foo = Foo { pk  :: !Int
               , _val :: !String
               } deriving (Eq, Ord, Data, Show)
  
type FooPkIx = Int
type FooIxs = '[Int, String]
type FooSet = IxSetPk Int FooIxs Foo

instance IsPkIx FooPkIx Foo where
  pkIndex = pkFun pk
instance IX.Indexable FooIxs Foo where
  indices = ixList (ixGen (Proxy :: Proxy Int))
                   (ixGen (Proxy :: Proxy String))


main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "allTests" $
  [ testSpec' "functionsOnEmpty" spec_functionsOnEmpty
  , testSpec' "adhoc"            spec_adhoc
  , testProperty  "roundtrips"       prop_roundtrips
  ]


-- these become properties later...
spec_functionsOnEmpty :: Spec
spec_functionsOnEmpty = do
  let fooSet :: FooSet = empty

  it "size is zero" $ do
    size fooSet `shouldBe` 0
  it "null is true" $ do
    IXP.null fooSet `shouldBe` True
  it "toList is empty" $ do
    toList fooSet `shouldBe` []
  it "toSet is empty" $ do
    toSet fooSet `shouldBe` S.empty
  it "toIxSet is empty" $ do
    IX.toSet (toIxSet fooSet) `shouldBe` S.empty

prop_roundtrips :: Property
prop_roundtrips = property False

spec_adhoc :: Spec
spec_adhoc = do
  it "adhoc" $
    False `shouldBe` True



  
