module Data.Binary.Get where

import Prelude
import Data.ArrayBuffer.DataView as DV
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, runST)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.ArrayBuffer.Typed (asFloat64Array, asFloat32Array, asUint8ClampedArray, asUint32Array, asUint16Array, asUint8Array, asInt32Array, asInt16Array, asInt8Array)
import Data.ArrayBuffer.Types (ArrayBuffer, Float64Array, Float32Array, Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array, ByteLength, DataView, ByteOffset)
import Data.Maybe (Maybe(..))
import Data.UInt (UInt)

data Decoder a = Fail String
               | Partial
               | Done a

type Getter a = forall e h. STRef h {dv :: DataView, off :: ByteOffset} -> Eff (st :: ST h, arrayBuffer :: ARRAY_BUFFER | e) (Decoder a)
type ArrayGetter a = ByteLength -> Getter a



getter :: forall a. ByteLength -> DV.Getter a -> Getter a
getter n f dref = do
  st <- modifySTRef dref $ \st -> {dv: st.dv, off: st.off + n}
  r <- f st.dv (st.off - n)
  pure $ case r of
    Just v -> Done v
    Nothing -> Partial

getI8 :: Getter Int
getI8 =  getter 1 DV.getInt8
getI16 :: Getter Int
getI16 = getter 2 DV.getInt16be
getI32 :: Getter Int
getI32 = getter 4 DV.getInt32be
getU8 :: Getter UInt
getU8 = getter 1 DV.getUint8
getU16 :: Getter UInt
getU16 = getter 2 DV.getUint16be
getU32 :: Getter UInt
getU32 = getter 4 DV.getUint32be
getF32 :: Getter Number
getF32 = getter 4 DV.getFloat32be
getF64 :: Getter Number
getF64 = getter 8 DV.getFloat64be

getDataView :: ByteLength -> Getter DataView
getDataView n = getter n getAt
  where getAt :: forall e. DataView -> ByteOffset -> Eff (arrayBuffer :: ARRAY_BUFFER | e) (Maybe DataView)
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

from :: forall e h. ArrayBuffer -> Eff (st :: ST h | e) (STRef h {dv :: DataView, off :: ByteOffset})
from ab = newSTRef { dv: DV.whole ab, off: 0 }

get :: forall a. ArrayBuffer -> Getter a -> Eff (arrayBuffer :: ARRAY_BUFFER) (Decoder a)
get ab f = runST (from ab >>= f)

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
