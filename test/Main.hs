module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List (foldl', sort)

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Unit Tests"
    [ testCase "3*5 == 15" $ 3*5 @?= 15
    , testCase "2*2 == 4" $ 4 @=? 2*2
    , testCase "zipLong [1,2,3] abc" $ zipLong [1,2,3] "abc" @?= [(1,'a'),(2,'b'),(3,'c')]
    , testCase "zipLong [1,2] abcd" $ zipLong "abcd" [1,2] @?= [('a',1),('b',2),('c',1),('d',2)]
    ]
  ]