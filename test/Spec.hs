-- module Main where

import Test.Hspec
import Control.Exception (evaluate)

import Fibonacci

main :: IO ()
main = hspec $ do
  describe "fib010Simple" $ do
    it "should return 0, for fib 0" $ do
      fib010Simple 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib010Simple 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib010Simple 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib010Simple (negate 1)) `shouldThrow` anyException

  describe "fib020Accumulator" $ do
    it "should return 0, for fib 0" $ do
      fib020Accumulator 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib020Accumulator 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib020Accumulator 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib020Accumulator (negate 1)) `shouldThrow` anyException

  describe "fib030Monadic" $ do
    it "should return 0, for fib 0" $ do
      fib030Monadic 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib030Monadic 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib030Monadic 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib030Monadic (negate 1)) `shouldThrow` anyException

  describe "fib040ILwithZip" $ do
    it "should return 0, for fib 0" $ do
      fib040ILwithZip 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib040ILwithZip 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib040ILwithZip 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib040ILwithZip (negate 1)) `shouldThrow` anyException

  describe "fib050ILdirectSelfRef" $ do
    it "should return 0, for fib 0" $ do
      fib050ILdirectSelfRef 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib050ILdirectSelfRef 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib050ILdirectSelfRef 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib050ILdirectSelfRef (negate 1)) `shouldThrow` anyException

  describe "fib060ILscanl" $ do
    it "should return 0, for fib 0" $ do
      fib060ILscanl 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib060ILscanl 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib060ILscanl 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib060ILscanl (negate 1)) `shouldThrow` anyException

  describe "fib070ILscanl2" $ do
    it "should return 0, for fib 0" $ do
      fib070ILscanl2 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib070ILscanl2 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib070ILscanl2 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib070ILscanl2 (negate 1)) `shouldThrow` anyException

  describe "fib080ILscanlFixGood" $ do
    it "should return 0, for fib 0" $ do
      fib080ILscanlFixGood 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib080ILscanlFixGood 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib080ILscanlFixGood 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib080ILscanlFixGood (negate 1)) `shouldThrow` anyException

  describe "fib081ILscanlFixBad" $ do
    it "should return 0, for fib 0" $ do
      fib081ILscanlFixBad 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib081ILscanlFixBad 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib081ILscanlFixBad 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib081ILscanlFixBad (negate 1)) `shouldThrow` anyException

  describe "fib082ILscanl2FixGood" $ do
    it "should return 0, for fib 0" $ do
      fib082ILscanl2FixGood 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib082ILscanl2FixGood 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib082ILscanl2FixGood 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib082ILscanl2FixGood (negate 1)) `shouldThrow` anyException

  describe "fib083ILscanl2FixBad" $ do
    it "should return 0, for fib 0" $ do
      fib083ILscanl2FixBad 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib083ILscanl2FixBad 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib083ILscanl2FixBad 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib083ILscanl2FixBad (negate 1)) `shouldThrow` anyException

  describe "fib090ILfoldr" $ do
    it "should return 0, for fib 0" $ do
      fib090ILfoldr 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib090ILfoldr 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib090ILfoldr 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib090ILfoldr (negate 1)) `shouldThrow` anyException

  describe "fib100ILiterate" $ do
    it "should return 0, for fib 0" $ do
      fib100ILiterate 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib100ILiterate 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib100ILiterate 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib100ILiterate (negate 1)) `shouldThrow` anyException

  describe "fib110identities" $ do
    it "should return 0, for fib 0" $ do
      fib110identities 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib110identities 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib110identities 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib110identities (negate 1)) `shouldThrow` anyException

  describe "fib120matrix" $ do
    it "should return 0, for fib 0" $ do
      fib120matrix 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib120matrix 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib120matrix 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib120matrix (negate 1)) `shouldThrow` anyException

  describe "fib130fast" $ do
    it "should return 0, for fib 0" $ do
      fib130fast 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib130fast 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib130fast 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib130fast (negate 1)) `shouldThrow` anyException

  describe "fib131faster" $ do
    it "should return 0, for fib 1" $ do
      fib131faster 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib131faster 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib131faster 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib131faster (negate 1)) `shouldThrow` anyException

  describe "fib132fastest" $ do
    it "should return 0, for fib 1" $ do
      fib132fastest  0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib132fastest  1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib132fastest  10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib132fastest  (negate 1)) `shouldThrow` anyException

  describe "fib140constant" $ do
    it "should return 0, for fib 1" $ do
      fib140constant  0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib140constant  1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib140constant  10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib140constant  (negate 1)) `shouldThrow` anyException
