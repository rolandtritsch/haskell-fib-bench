{-# LANGUAGE BangPatterns #-}

{-|
Module      : Fibonacci
Description : Various implementations of the Fibonacci sequence.
Copyright   : (c) Roland Tritsch, 2017
License     : GPL-3
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

Found a couple of [implementations](https://wiki.haskell.org/The_Fibonacci_sequence)
for the [Fibonacci](https://en.wikipedia.org/wiki/Fibonacci_number) sequence and decide to
benchmark them.
-}
module Fibonacci where

import Data.List
import Data.Bits

import Control.Monad.State
import Control.Exception

data InvalidParameterException = InvalidParameterException deriving (Show)
instance Exception InvalidParameterException

-- | Most simple (and slowest) implementation (O(fib n) additions).
fib010Simple :: Int -> Int
fib010Simple 0 = 0
fib010Simple 1 = 1
fib010Simple n = assert (n >= 2) (fib010Simple (n - 1) + fib010Simple (n - 2))

-- | Using accumulator for state passing.
fib020Accumulator :: Int -> Int
fib020Accumulator n = assert (n >= 0) go n (0, 1) where
  go !n' (!a, !b)
    | n' == 0 = a
    | otherwise = go (n' - 1) (b, a + b)

-- | Monadic.
fib030Monadic :: Int -> Int
fib030Monadic n = assert (n >= 0) flip evalState (0, 1) $ do
  _ <- ($) forM [0 .. (n - 1)] $ \_ -> do
    (a, b) <- get
    put (b, a + b)
  (a, _) <- get
  return a

-- | Infinite list. Using zipWith.
fib040ILwithZip :: Int -> Int
fib040ILwithZip n = assert (n >= 0) fibs !! n where
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Infinite list. Using direct self-reference.
fib050ILdirectSelfRef :: Int -> Int
fib050ILdirectSelfRef n = assert (n >= 0) fibs !! n where
  fibs = 0 : 1 : next fibs where
    -- next [] = assert (False) 0
    -- next [_] = assert (False) 0
    next (a : t@(b:_)) = (a + b) : next t

-- | Infite list. Using scanl.
fib060ILscanl :: Int -> Int
fib060ILscanl n = assert (n >= 0) fibs !! n where
  fibs = scanl (+) 0 (1 : fibs)

-- | Infinite list. Using scanl. Again.
fib070ILscanl2 :: Int -> Int
fib070ILscanl2 n = assert (n >= 0) fibs !! n where
  fibs = 0 : scanl (+) 1 fibs

-- | Good implementation of fix.
fixGood :: (t -> t) -> t
fixGood f = xs where
  xs = f xs

-- | Bad implementation of fix. Will show quadratic behaviour.
fixBad :: (t -> t) -> t
fixBad f = f (fix f)

-- | Infinite list. Using scanl. With fixGood.
fib080ILscanlFixGood :: Int -> Int
fib080ILscanlFixGood n = assert (n >= 0) fibs !! n where
  fibs = fixGood (scanl (+) 0 . (1:))

-- | Infinite list. Using scanl. With fixBad.
fib081ILscanlFixBad :: Int -> Int
fib081ILscanlFixBad n = assert (n >= 0) fibs !! n where
  fibs = fixBad (scanl (+) 0 . (1:))

-- | Infinite list. Using scanl. Again. With fixGood.
fib082ILscanl2FixGood :: Int -> Int
fib082ILscanl2FixGood n = assert (n >= 0) fibs !! n where
  fibs = fixGood ((0:) . scanl (+) 1)

-- | Infinite list. Using scanl. Again. With fixBad.
fib083ILscanl2FixBad :: Int -> Int
fib083ILscanl2FixBad n = assert (n >= 0) fibs !! n where
  fibs = fixBad ((0:) . scanl (+) 1)

-- ! Infinite list. Using foldr.
fib090ILfoldr :: Int -> Int
fib090ILfoldr n = assert (n >= 0) fibs !! n where
  fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

-- ! Infinite list. Using iterate.
fib100ILiterate :: Int -> Int
fib100ILiterate n = assert (n >= 0) fibs !! n where
  fibs = map fst $ iterate (\(a, b) -> (b, (a + b))) (0, 1)

-- | Using identities.
fib110identities :: Int -> Int
fib110identities 0 = 0
fib110identities 1 = 1
fib110identities n
  | n < 0 = throw InvalidParameterException
  | even n = f1 * (f1 + 2 * f2)
  | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
  | otherwise = (2 * f1 + f2) * (2 * f1 - f2) - 2
  where
    k = n `div` 2
    f1 = fib110identities k
    f2 = fib110identities (k - 1)

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)
instance Num a => Num (Matrix a) where
  Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
  Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
  Matrix as * Matrix bs = Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
  negate (Matrix as) = Matrix (map (map negate) as)
  fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
  abs m = m
  signum _ = 1

apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]

-- | Using a [matrix](https://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form).
fib120matrix :: Int -> Int
fib120matrix n = assert (n >= 0) head (apply (Matrix [[0, 1], [1, 1]] ^ n) [0, 1])

-- | Fast fib.
fib130fast :: Int -> Int
fib130fast 0 = 0
fib130fast n
  | n < 0 = throw InvalidParameterException
  | otherwise = (fst . fib) (n - 1) where
    fib 0 = (1, 1)
    fib 1 = (1, 2)
    fib n
      | even n = (a*a + b*b, c*c - a*a)
      | otherwise = (c*c - a*a, b*b + c*c)
      where
        (a, b) = fib (n `div` 2 - 1)
        c = a +b

-- | Faster fib.
fib131faster :: Int -> Int
fib131faster n = assert (n >= 0) snd . foldl fib (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n where
  unfoldl f x = case f x of
    Just (u, v) -> unfoldl f v ++ [u]
    Nothing -> []

  divs 0 = Nothing
  divs k = Just (uncurry (flip (,)) (k `divMod` 2))

  fib (f, g) p
    | p = (f * (f + 2*g), ss)
    | otherwise = (ss, g * (2*f - g))
    where
      ss = f*f + g*g

-- | Fastest fib.
fib132fastest  :: Int -> Int
fib132fastest  n = assert (n >= 0) snd . foldl_ fib_ (1, 0) . dropWhile not $ [testBit n k | k <- let s = finiteBitSize n in [s - 1, s - 2..0]] where
  fib_ (f, g) p
    | p = (f * (f + 2*g), ss)
    | otherwise = (ss, g * (2*f - g))
    where
      ss = f*f + g*g

  foldl_ = foldl' --

-- | Constant time. Using binet.
fib140constant  :: Int -> Int
fib140constant  n = round $ phi ** fromIntegral n / sq5 where
  sq5 = sqrt 5 :: Double
  phi = (1 + sq5) / 2
