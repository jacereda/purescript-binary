module Test.Main where

import Test.QuickCheck
import Data.ArrayBuffer.DataView as DV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Binary.Class (load, class Loadable, save, class Saveable)
import Data.Binary.Get (from, getF32, getF64, getU32, getI32, getI16, getU16, getU8, Decoder(..), get, getI8)
import Data.Binary.Put (putF64, putF32, putI32, putU32, putI16, putU16, putU8, put, putI8)
import Data.Int.Bits (zshr, (.&.))
import Prelude ((<<<), (/=), class Show, class Eq, Unit, pure, bind, show, (==), (>>=), (<*>), (<$>), ($), (<>), (&&), (-), (*))
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)

newtype TInt8 = TInt8 Int

instance eqTInt8 :: Eq TInt8 where
  eq (TInt8 a) (TInt8 b) = a == b

instance showTInt8 :: Show TInt8 where
  show (TInt8 a)  = show a

instance arbTInt8 :: Arbitrary TInt8 where
  arbitrary = TInt8 <$> x
    where x = (_ .&. 255 - 128) <$> arbitrary

instance saveableTInt8 :: Saveable TInt8 where
  save (TInt8 x) = putI8 x

instance loadableTInt8 :: Loadable TInt8 where
  load d = do
    r <- getI8 d
    pure $ TInt8 <$> r


newtype TUInt8 = TUInt8 Int

instance eqTUInt8 :: Eq TUInt8 where
  eq (TUInt8 a) (TUInt8 b) = a == b

instance showTUInt8 :: Show TUInt8 where
  show (TUInt8 a)  = show a

instance arbTUInt8 :: Arbitrary TUInt8 where
  arbitrary = TUInt8 <$> x
    where x = (_ .&. 255) <$> arbitrary

instance saveableTUInt8 :: Saveable TUInt8 where
  save (TUInt8 x) = putU8 x

instance loadableTUInt8 :: Loadable TUInt8 where
  load d = do
    r <- getU8 d
    pure $ TUInt8 <$> r


newtype TInt16 = TInt16 Int

instance eqTInt16 :: Eq TInt16 where
  eq (TInt16 a) (TInt16 b) = a == b

instance showTInt16 :: Show TInt16 where
  show (TInt16 a)  = show a

instance arbTInt16 :: Arbitrary TInt16 where
  arbitrary = TInt16 <$> x
    where x = (_ .&. 65535 - 32768) <$> arbitrary

instance saveableTInt16 :: Saveable TInt16 where
  save (TInt16 x) = putI16 x

instance loadableTInt16 :: Loadable TInt16 where
  load d = do
    r <- getI16 d
    pure $ TInt16 <$> r


newtype TUInt16 = TUInt16 Int

instance eqTUInt16 :: Eq TUInt16 where
  eq (TUInt16 a) (TUInt16 b) = a == b

instance showTUInt16 :: Show TUInt16 where
  show (TUInt16 a)  = show a

instance arbTUInt16 :: Arbitrary TUInt16 where
  arbitrary = TUInt16 <$> x
    where x = (_ .&. 65535) <$> arbitrary

instance saveableTUInt16 :: Saveable TUInt16 where
  save (TUInt16 x) = putU16 x

instance loadableTUInt16 :: Loadable TUInt16 where
  load d = do
    r <- getU16 d
    pure $ TUInt16 <$> r


newtype TInt32 = TInt32 Int

instance eqTInt32 :: Eq TInt32 where
  eq (TInt32 a) (TInt32 b) = a == b

instance showTInt32 :: Show TInt32 where
  show (TInt32 a)  = show a

instance arbTInt32 :: Arbitrary TInt32 where
  arbitrary = TInt32 <$> arbitrary

instance saveableTInt32 :: Saveable TInt32 where
  save (TInt32 x) = putI32 x

instance loadableTInt32 :: Loadable TInt32 where
  load d = do
    r <- getI32 d
    pure $ TInt32 <$> r

newtype TUInt32 = TUInt32 Int

instance eqTUInt32 :: Eq TUInt32 where
  eq (TUInt32 a) (TUInt32 b) = a == b

instance showTUInt32 :: Show TUInt32 where
  show (TUInt32 a)  = show a

instance arbTUInt32 :: Arbitrary TUInt32 where
  arbitrary = TUInt32 <$> x
    where x = (_ `zshr` 0) <$> arbitrary

instance saveableTUInt32 :: Saveable TUInt32 where
  save (TUInt32 x) = putU32 x

instance loadableTUInt32 :: Loadable TUInt32 where
  load d = do
    r <- getU32 d
    pure $ TUInt32 <$> r
    


newtype TF32 = TF32 Number

instance eqTF32 :: Eq TF32 where
  eq (TF32 a) (TF32 b) = a == b

instance showTF32 :: Show TF32 where
  show (TF32 a)  = show a

instance arbTF32 :: Arbitrary TF32 where
  arbitrary = TF32 <$> x
    where x = fround <$> arbitrary

instance saveableTF32 :: Saveable TF32 where
  save (TF32 x) = putF32 x

instance loadableTF32 :: Loadable TF32 where
  load d = do
    r <- getF32 d
    pure $ TF32 <$> r


newtype TF64 = TF64 Number

instance eqTF64 :: Eq TF64 where
  eq (TF64 a) (TF64 b) = a == b

instance showTF64 :: Show TF64 where
  show (TF64 a)  = show a

instance arbTF64 :: Arbitrary TF64 where
  arbitrary = TF64 <$> arbitrary

instance saveableTF64 :: Saveable TF64 where
  save (TF64 x) = putF64 x

instance loadableTF64 :: Loadable TF64 where
  load d = do
    r <- getF64 d
    pure $ TF64 <$> r



data V4 a = V4 a a a a

instance eqV4 :: Eq a => Eq (V4 a) where
  eq (V4 x0 y0 z0 t0) (V4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1

instance showV4 :: Show a => Show (V4 a) where
  show (V4 x y z t) = "(V4 " <> show x <> " " <> show y <> " " <> show z <> " " <> show t <> ")"
  
instance arbV4 :: Arbitrary a => Arbitrary (V4 a) where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance saveableV4 :: Saveable a => Saveable (V4 a) where
  save (V4 x y z t) d = pure d >>= save x >>= save y >>= save z >>= save t

instance loadableV4 :: Loadable a => Loadable (V4 a) where
  load d = do
    x <- comp
    y <- comp
    z <- comp
    t <- comp
    pure $ V4 <$> x <*> y <*> z <*> t
    where comp = load d
          
data M4 a = M4 (V4 a) (V4 a) (V4 a) (V4 a)

instance eqM4 :: Eq a => Eq (M4 a) where
  eq (M4 x0 y0 z0 t0) (M4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1

instance showM4 :: Show a => Show (M4 a) where
  show (M4 x y z t) = "M4 " <> show x <> " " <> show y <> " " <> show z <> " " <> show t

instance arbM4 :: Arbitrary a => Arbitrary (M4 a) where
  arbitrary = M4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance saveableM4 :: Saveable a => Saveable (M4 a) where
  save (M4 x y z t) d = pure d >>= save x >>= save y >>= save z >>= save t

instance loadableM4 :: Loadable a => Loadable (M4 a) where
  load d = do
    x <- comp
    y <- comp
    z <- comp
    t <- comp
    pure $ M4 <$> x <*> y <*> z <*> t
    where comp = load d

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION, writer :: DV.WRITER | e) Unit
main = do
  quickCheck \m -> mat m <?> "serialising Int8 matrix" <> show (m :: M4 TInt8)
  quickCheck \m -> mat m <?> "serialising Int16 matrix" <> show (m :: M4 TInt16)
  quickCheck \m -> mat m <?> "serialising Int32 matrix" <> show (m :: M4 TInt32)
  quickCheck \m -> mat m <?> "serialising UInt8 matrix" <> show (m :: M4 TUInt8)  
  quickCheck \m -> mat m <?> "serialising UInt16 matrix" <> show (m :: M4 TUInt16)  
  quickCheck \m -> mat m <?> "serialising UInt32 matrix" <> show (m :: M4 TUInt32)  
  quickCheck \m -> mat m <?> "serialising F32 matrix" <> show (m :: M4 TF32)
  quickCheck \m -> mat m <?> "serialising F64 matrix" <> show (m :: M4 TF64)  
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising Int8 matrix"
                             <> show (m0 :: M4 TInt8)
                             <> show (m1 :: M4 TInt8)
                             <> show (m2 :: M4 TInt8)
                             <> show (m3 :: M4 TInt8)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising Int16 matrix"
                             <> show (m0 :: M4 TInt16)
                             <> show (m1 :: M4 TInt16)
                             <> show (m2 :: M4 TInt16)
                             <> show (m3 :: M4 TInt16)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising Int32 matrix"
                             <> show (m0 :: M4 TInt32)
                             <> show (m1 :: M4 TInt32)
                             <> show (m2 :: M4 TInt32)
                             <> show (m3 :: M4 TInt32)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising UInt8 matrix"
                             <> show (m0 :: M4 TUInt8)
                             <> show (m1 :: M4 TUInt8)
                             <> show (m2 :: M4 TUInt8)
                             <> show (m3 :: M4 TUInt8)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising UInt16 matrix"
                             <> show (m0 :: M4 TUInt16)
                             <> show (m1 :: M4 TUInt16)
                             <> show (m2 :: M4 TUInt16)
                             <> show (m3 :: M4 TUInt16)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising UInt32 matrix"
                             <> show (m0 :: M4 TUInt32)
                             <> show (m1 :: M4 TUInt32)
                             <> show (m2 :: M4 TUInt32)
                             <> show (m3 :: M4 TUInt32)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising F32 matrix"
                             <> show (m0 :: M4 TF32)
                             <> show (m1 :: M4 TF32)
                             <> show (m2 :: M4 TF32)
                             <> show (m3 :: M4 TF32)
  quickCheck \m0 m1 m2 m3 -> serdes m0 m1 m2 m3 <?> "serialising/deserialising F64 matrix"
                             <> show (m0 :: M4 TF64)
                             <> show (m1 :: M4 TF64)
                             <> show (m2 :: M4 TF64)
                             <> show (m3 :: M4 TF64)
  quickCheck \m1 m2 -> short m1 m2 <?> "short read"
                       <> show (m1 :: M4 TUInt8)
                       <> show (m2 :: M4 TUInt8)


mat :: forall a. (Loadable a, Saveable a, Eq a, Arbitrary a) => M4 a -> Boolean
mat m = let ab = put 128 \s -> pure s >>= save m in
  case get load ab of
    Done m' -> m' == m
    _ -> false

serdes :: forall a. (Loadable a, Saveable a, Eq a) => M4 a -> M4 a -> M4 a -> M4 a -> Boolean
serdes m0 m1 m2 m3 = unsafePerformEff $ do
    let a = put 512 \s -> pure s >>= save m0 >>= save m1 >>= save m2 >>= save m3
    d <- from a
    let g = load d
    m0' <- g
    m1' <- g
    m2' <- g
    m3' <- g
    pure $ (Done m0) == m0' && (Done m1) == m1' && (Done m2) == m2' && (Done m3) == m3'

short :: forall a. (Loadable a, Saveable a, Eq a) => (M4 a) -> (M4 a) -> Boolean
short m0 m1 = unsafePerformEff $ do
    let a = put 256 \s -> save m0 s
    d <- from a
    let g = load d
    m0' <- g
    m1' <- g
    pure $ m0' == (Done m0) && m1' /= (Done m1) && m1' == Partial


foreign import fround :: Number -> Number
