module Test.Main where

import Debug.Trace
import Debug.Foreign
import Data.Either
import Data.Maybe
import Test.QuickCheck
import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import qualified Data.ArrayBuffer.Typed as TA
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Math

newtype Comp = Comp Number

putComp :: Putter Comp
putComp (Comp v) = putI8 v

getComp :: Getter Comp
getComp d = do
  r <- getI8 d
  return $ Comp <$> r

instance eqComp :: Eq Comp where
  (==) (Comp v0) (Comp v1) = v0 == v1
  (/=) a b = not $ a == b

instance showComp :: Show Comp where
  show (Comp v) = "Comp " ++ show v

instance arbComp :: Arbitrary Comp where
  arbitrary = uniformToComp <$> arbitrary
    where
    uniformToComp n = Comp $ Math.floor (n * 256) - 128

data V4 = V4 Comp Comp Comp Comp

instance eqV4 :: Eq V4 where
  (==) (V4 x0 y0 z0 t0) (V4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1
  (/=) a b = not $ a == b

instance showV4 :: Show V4 where
  show (V4 x y z t) = "(V4 " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show t ++ ")"
  
instance arbV4 :: Arbitrary V4 where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

putV4 :: Putter V4
putV4 (V4 x y z t) d = pure d >>= putComp x >>= putComp y >>= putComp z >>= putComp t

getV4 :: Getter V4
getV4 d = do
  x <- comp
  y <- comp
  z <- comp
  t <- comp
  return $ V4 <$> x <*> y <*> z <*> t
  where comp = getComp d
          
data M4 = M4 V4 V4 V4 V4

instance eqM4 :: Eq M4 where
  (==) (M4 x0 y0 z0 t0) (M4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1
  (/=) a b = not $ a == b

instance showM4 :: Show M4 where
  show (M4 x y z t) = "M4 " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show t

instance arbM4 :: Arbitrary M4 where
  arbitrary = M4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  
putM4 :: Putter M4
putM4 (M4 x y z t) d = pure d >>= putV4 x >>= putV4 y >>= putV4 z >>= putV4 t

getM4 :: Getter M4
getM4 d = do
  x <- comp
  y <- comp
  z <- comp
  t <- comp
  return $ M4 <$> x <*> y <*> z <*> t
  where comp = getV4 d

main :: Eff (trace :: Trace, random :: Random, err :: Exception, writer :: DV.Writer) Unit
main = do
  quickCheck mat
  quickCheck serdes
  quickCheck short


mat :: M4 -> Boolean
mat m = let ab = put 256 \s -> pure s >>= putM4 m in
  case get getM4 ab of
    Done m' -> m' == m
    _ -> false

serdes :: M4 -> M4 -> M4 -> M4 -> Boolean
serdes m0 m1 m2 m3 = forcePure $ do
    let a = put 256 \s -> pure s >>= putM4 m0 >>= putM4 m1 >>= putM4 m2 >>= putM4 m3
    d <- from a
    let g = getM4 d
    m0' <- g
    m1' <- g
    m2' <- g
    m3' <- g
    return $ (Done m0) == m0' && (Done m1) == m1' && (Done m2) == m2' && (Done m3) == m3'

short :: M4 -> M4 -> Boolean
short m0 m1 = forcePure $ do
    let a = put 256 \s -> putM4 m0 s
    d <- from a
    let g = getM4 d
    m0' <- g
    m1' <- g
    return $ m0' == (Done m0) && m1' /= (Done m1) && m1' == Partial

assert :: Boolean -> QC Unit
assert = quickCheck' 1

foreign import forcePure "function forcePure(e) { return e(); }" :: forall e. (Eff (|e) Boolean) -> Boolean

