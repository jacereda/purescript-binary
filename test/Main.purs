module Test.Main where

import Test.QuickCheck
import Data.ArrayBuffer.DataView as DV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Binary.Get (from, Getter, Decoder(..), get, getI8)
import Data.Binary.Put (put, Putter, putI8)
import Data.Int.Bits ((.&.))
import Prelude ((/=), class Show, class Eq, Unit, pure, bind, show, (==), (>>=), (<*>), (<$>), ($), (<>), (&&), (-), (*))
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)

newtype Comp = Comp Int

putComp :: Putter Comp
putComp (Comp v) = putI8 v

getComp :: Getter Comp
getComp d = do
  r <- getI8 d
  pure $ Comp <$> r

instance eqComp :: Eq Comp where
  eq (Comp v0) (Comp v1) = v0 == v1

instance showComp :: Show Comp where
  show (Comp v) = "Comp " <> show v

instance arbComp :: Arbitrary Comp where
  arbitrary = uniformToComp <$> arbitrary
    where
    uniformToComp n = Comp $ (n .&. 255) - 128

data V4 = V4 Comp Comp Comp Comp

instance eqV4 :: Eq V4 where
  eq (V4 x0 y0 z0 t0) (V4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1

instance showV4 :: Show V4 where
  show (V4 x y z t) = "(V4 " <> show x <> " " <> show y <> " " <> show z <> " " <> show t <> ")"
  
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
  pure $ V4 <$> x <*> y <*> z <*> t
  where comp = getComp d
          
data M4 = M4 V4 V4 V4 V4

instance eqM4 :: Eq M4 where
  eq (M4 x0 y0 z0 t0) (M4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1

instance showM4 :: Show M4 where
  show (M4 x y z t) = "M4 " <> show x <> " " <> show y <> " " <> show z <> " " <> show t

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
  pure $ M4 <$> x <*> y <*> z <*> t
  where comp = getV4 d

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION, writer :: DV.WRITER | e) Unit
main = do
  quickCheck \m -> mat m <?> "serialising " <> show m
  quickCheck \b -> unsafePerformEff $ booltest b
  quickCheck serdes
--  quickCheck short


booltest :: forall e. Boolean -> Eff (|e) Boolean
booltest b = pure $ true

mat :: M4 -> Boolean
mat m = let ab = put 256 \s -> pure s >>= putM4 m in
  case get getM4 ab of
    Done m' -> m' == m
    _ -> false

serdes :: M4 -> M4 -> M4 -> M4 -> Boolean
serdes m0 m1 m2 m3 = unsafePerformEff $ do
    let a = put 256 \s -> pure s >>= putM4 m0 >>= putM4 m1 >>= putM4 m2 >>= putM4 m3
    d <- from a
    let g = getM4 d
    m0' <- g
    m1' <- g
    m2' <- g
    m3' <- g
    pure $ (Done m0) == m0' && (Done m1) == m1' && (Done m2) == m2' && (Done m3) == m3'

short :: M4 -> M4 -> Boolean
short m0 m1 = unsafePerformEff $ do
    let a = put 256 \s -> putM4 m0 s
    d <- from a
    let g = getM4 d
    m0' <- g
    m1' <- g
    pure $ m0' == (Done m0) && m1' /= (Done m1) && m1' == Partial


