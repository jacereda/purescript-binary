module Data.Binary.Get where

import Data.Either
import Data.Maybe
import Control.Monad.Eff
import Data.ArrayBuffer.Types
import Data.ArrayBuffer.Typed
import qualified Data.ArrayBuffer.DataView as DV
import Data.Binary.Advancer

data Decoder a = Fail
               | Partial
               | Done a

type GetState = Advancer
type Getter a = GetState -> Eff (reader :: DV.Reader) (Decoder a)
type ArrayGetter a = Number -> Getter a

getter :: forall a. Number -> DV.Getter a -> Getter a
getter n f d = do
  o <- advance n d
  r <- f d.dv o
  return $ case r of
    Just v -> Done v
    otherwise -> Partial

getInt8 :: Getter Int8
getInt8 =  getter 1 DV.getInt8
getInt16 :: Getter Int16
getInt16 = getter 2 DV.getInt16
getInt32 :: Getter Int32
getInt32 = getter 4 DV.getInt32
getUint8 :: Getter Uint8
getUint8 = getter 1 DV.getUint8
getUint16 :: Getter Uint16
getUint16 = getter 2 DV.getUint16
getUint32 :: Getter Uint32
getUint32 = getter 4 DV.getUint32
getFloat32 :: Getter Float32
getFloat32 = getter 4 DV.getFloat32
getFloat64 :: Getter Float64
getFloat64 = getter 8 DV.getFloat64

getDataView :: ByteLength -> Getter DataView
getDataView n = getter n get
  where get :: forall e. DataView -> ByteOffset -> Eff (reader :: DV.Reader | e) (Maybe DataView)
        get d off = return $ DV.slice off n (DV.buffer d)

getTypedArray :: forall t. (DataView -> t) -> ByteLength -> ArrayGetter t
getTypedArray conv sizeof n d = do
  r <- getDataView (sizeof * n) d
  return $ conv <$> r

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

open :: ArrayBuffer -> Eff (reader :: DV.Reader) GetState
open ab = return $ { dv : DV.whole ab, off : 0 }

get :: forall a. Getter a -> ArrayBuffer -> Decoder a
get f b = runRPure (open b >>= f)

foreign import runRPure
"""
function runRPure(f) {
  return f();
}
""" :: forall e a. Eff (|e) a -> a

instance applicativeDecoder :: Applicative Decoder where
  pure = Done

instance functorDecoder :: Functor Decoder where
  (<$>) f (Done v) = Done (f v)
  (<$>) _ Partial = Partial
  (<$>) _ _ = Fail

instance applyDecoder :: Apply Decoder where
  (<*>) (Done f) (Done x) = Done $ f x
  (<*>) Partial _ = Partial
  (<*>) _ Partial = Partial
  (<*>) _ _ = Fail

instance eqDecoder :: (Eq a) => Eq (Decoder a) where
  (==) (Done a) (Done b) = a == b
  (==) Partial Partial = true
  (==) Fail Fail = true
  (==) _ _ = false
  (/=) a b = not $ a == b
  
