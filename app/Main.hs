{-# LANGUAGE BangPatterns #-}

{-|
Module      : Main
Description : Main module to run benchmark on the Fibonacci sequence.
Copyright   : (c) Roland Tritsch, 2017
License     : GPL-3
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

While I was working through [The Haskell Book](http://haskellbook.com/) I
stumbled over [Criterion](https://hackage.haskell.org/package/criterion) and
wanted to learn and understand more how it works.
-}
module Main where

import Criterion.Main

import Fibonacci

suite :: [Benchmark]
suite = [
  bgroup "simple" [
    bench "fib 5" $ whnf fib010Simple 5,
    bench "fib 10" $ whnf fib010Simple 10
    -- cannot bench this. Too slow. It makes the report look bad.
    --bench "fib 20" $ whnf fib010Simple 20
    ],
  bgroup "accumulator" [
    bench "fib 5" $ whnf fib020Accumulator 5,
    bench "fib 10" $ whnf fib020Accumulator 10,
    bench "fib 20" $ whnf fib020Accumulator 20
    ],
  bgroup "monadic" [
    bench "fib 5" $ whnf fib030Monadic 5,
    bench "fib 10" $ whnf fib030Monadic 10,
    bench "fib 20" $ whnf fib030Monadic 20
    ],
  bgroup "zipWith" [
    bench "fib 5" $ whnf fib040ILwithZip 5,
    bench "fib 10" $ whnf fib040ILwithZip 10,
    bench "fib 20" $ whnf fib040ILwithZip 20
    ],
  bgroup "self" [
    bench "fib 5" $ whnf fib050ILdirectSelfRef 5,
    bench "fib 10" $ whnf fib050ILdirectSelfRef 10,
    bench "fib 20" $ whnf fib050ILdirectSelfRef 20
    ],
  bgroup "scanl" [
    bench "fib 5" $ whnf fib060ILscanl 5,
    bench "fib 10" $ whnf fib060ILscanl 10,
    bench "fib 20" $ whnf fib060ILscanl 20
    ],
  bgroup "scanl2" [
    bench "fib 5" $ whnf fib070ILscanl2 5,
    bench "fib 10" $ whnf fib070ILscanl2 10,
    bench "fib 20" $ whnf fib070ILscanl2 20
    ],
  bgroup "scanl-fixGood" [
    bench "fib 5" $ whnf fib080ILscanlFixGood 5,
    bench "fib 10" $ whnf fib080ILscanlFixGood 10,
    bench "fib 20" $ whnf fib080ILscanlFixGood 20
    ],
  bgroup "scanl-fixBad" [
    bench "fib 5" $ whnf fib081ILscanlFixBad 5,
    bench "fib 10" $ whnf fib081ILscanlFixBad 10,
    bench "fib 20" $ whnf fib081ILscanlFixBad 20
    ],
  bgroup "scanl2-fixGood" [
    bench "fib 5" $ whnf fib082ILscanl2FixGood 5,
    bench "fib 10" $ whnf fib082ILscanl2FixGood 10,
    bench "fib 20" $ whnf fib082ILscanl2FixGood 20
    ],
  bgroup "scanl2-fixBad" [
    bench "fib 5" $ whnf fib083ILscanl2FixBad 5,
    bench "fib 10" $ whnf fib083ILscanl2FixBad 10,
    bench "fib 20" $ whnf fib083ILscanl2FixBad 20
    ],
  bgroup "unfoldr" [
    bench "fib 5" $ whnf fib090ILfoldr 5,
    bench "fib 10" $ whnf fib090ILfoldr 10,
    bench "fib 20" $ whnf fib090ILfoldr 20
    ],
  bgroup "iterate" [
    bench "fib 5" $ whnf fib100ILiterate 5,
    bench "fib 10" $ whnf fib100ILiterate 10,
    bench "fib 20" $ whnf fib100ILiterate 20
    ],
  bgroup "identities" [
    bench "fib 5" $ whnf fib110identities 5,
    bench "fib 10" $ whnf fib110identities 10,
    bench "fib 20" $ whnf fib110identities 20
    ],
  bgroup "matrix" [
    bench "fib 5" $ whnf fib120matrix 5,
    bench "fib 10" $ whnf fib120matrix 10,
    bench "fib 20" $ whnf fib120matrix 20
    ],
  bgroup "fast" [
    bench "fib 5" $ whnf fib130fast 5,
    bench "fib 10" $ whnf fib130fast 10,
    bench "fib 20" $ whnf fib130fast 20
    ],
  bgroup "faster" [
    bench "fib 5" $ whnf fib131faster 5,
    bench "fib 10" $ whnf fib131faster 10,
    bench "fib 20" $ whnf fib131faster 20
    ],
  bgroup "fastest" [
    bench "fib 5" $ whnf fib132fastest  5,
    bench "fib 10" $ whnf fib132fastest  10,
    bench "fib 20" $ whnf fib132fastest  20
    ],
  bgroup "constant" [
    bench "fib 5" $ whnf fib140constant  5,
    bench "fib 10" $ whnf fib140constant  10,
    bench "fib 20" $ whnf fib140constant  20
    ]
  ]

main :: IO ()
main = defaultMain suite
