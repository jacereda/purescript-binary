module Data.Binary.Get where

import Prelude
import Data.Maybe(Maybe(..))
import Control.Monad.Eff(Eff)
import Data.ArrayBuffer.Types (ArrayBuffer, Float64Array, Float32Array, Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array, ByteLength, DataView, ByteOffset)
import Data.ArrayBuffer.Typed (asFloat64Array, asFloat32Array, asUint8ClampedArray, asUint32Array, asUint16Array, asUint8Array, asInt32Array, asInt16Array, asInt8Array)
import Data.ArrayBuffer.DataView as DV
import Data.Binary.Advancer (Advancer, advance)

data Decoder a = Fail String
               | Partial
               | Done a

type GetState = Advancer
type Getter a = forall e. GetState -> Eff (reader :: DV.READER | e) (Decoder a)
type ArrayGetter a = Int -> Getter a

getter :: forall a. Int -> DV.Getter a -> Getter a
getter n f d = do
  o <- advance n d
  r <- f d.dv o
  pure $ case r of
    Just v -> Done v
    _ -> Partial

getI8 :: Getter Int
getI8 =  getter 1 DV.getInt8
getI16 :: Getter Int
getI16 = getter 2 DV.getInt16
getI32 :: Getter Int
getI32 = getter 4 DV.getInt32
getU8 :: Getter Int
getU8 = getter 1 DV.getUint8
getU16 :: Getter Int
getU16 = getter 2 DV.getUint16
getU32 :: Getter Int
getU32 = getter 4 DV.getUint32
getF32 :: Getter Number
getF32 = getter 4 DV.getFloat32
getF64 :: Getter Number
getF64 = getter 8 DV.getFloat64

getDataView :: ByteLength -> Getter DataView
getDataView n = getter n getAt
  where getAt :: forall e. DataView -> ByteOffset -> Eff (reader :: DV.READER | e) (Maybe DataView)
        getAt d off = pure $ DV.slice off n (DV.buffer d)

getTypedArray :: forall t. (DataView -> t) -> ByteLength -> ArrayGetter t
getTypedArray conv sizeof n d = do
  r <- getDataView (sizeof * n) d
  pure $ conv <$> r

getInt8Array :: ArrayGetter Int8Array
getInt8Array = getTypedArray asInt8Array 1
getInt16Array :: ArrayGetter Int16Array
getInt16Array = getTypedArray asInt16Array 2
getInt32Array :: ArrayGetter Int32Array
getInt32Array = getTypedArray asInt32Array 4
getUint8Array :: ArrayGetter Uint8Array
getUint8Array = getTypedArray asUint8Array 1
getUint16Array :: ArrayGetter Uint16Array
getUint16Array = getTypedArray asUint16Array 2
getUint32Array :: ArrayGetter Uint32Array
getUint32Array = getTypedArray asUint32Array 4
getUint8ClampedArray :: ArrayGetter Uint8ClampedArray
getUint8ClampedArray = getTypedArray asUint8ClampedArray 1
getFloat32Array :: ArrayGetter Float32Array
getFloat32Array = getTypedArray asFloat32Array 4
getFloat64Array :: ArrayGetter Float64Array
getFloat64Array = getTypedArray asFloat64Array 8

from :: forall e. ArrayBuffer -> Eff (reader :: DV.READER | e) GetState
from ab = pure $ { dv : DV.whole ab, off : 0 }

get :: forall a. Getter a -> ArrayBuffer -> Decoder a
get f b = runRPure (from b >>= f)

foreign import runRPure :: forall e a. Eff (|e) a -> a

instance applicativeDecoder :: Applicative Decoder where
  pure = Done

instance functorDecoder :: Functor Decoder where
  map f (Done v) = Done (f v)
  map _ Partial = Partial
  map _ (Fail r) = Fail r

instance applyDecoder :: Apply Decoder where
  apply (Done f) (Done x) = Done $ f x
  apply Partial _ = Partial
  apply _ Partial = Partial
  apply (Fail r) _ = Fail r
  apply _ (Fail r) = Fail r

instance eqDecoder :: (Eq a) => Eq (Decoder a) where
  eq (Done a) (Done b) = a == b
  eq Partial Partial = true
  eq (Fail a) (Fail b) = a == b
  eq _ _ = false
  
instance showDecoder :: (Show a) => Show (Decoder a) where
  show (Done v) = "(Done " <> show v <> ")"
  show Partial = "Partial"
  show (Fail r) = "(Fail " <> show r <> ")"
