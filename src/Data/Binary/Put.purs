module Data.Binary.Put where

import Prelude
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, DataView, ByteOffset)
import Data.Maybe (Maybe)
import Data.UInt (UInt)

newtype PutState = PutState { dv :: DataView, off :: ByteOffset }
type Putter a = a -> PutState -> Eff (arrayBuffer :: ARRAY_BUFFER) PutState

putter :: forall a. Int -> DV.Setter a -> Putter a
putter n f v (PutState s) = do
  f s.dv v s.off
  pure $ PutState { dv : s.dv, off : s.off + n }

putI8 :: Putter Int
putI8 = putter 1 DV.setInt8
putI16 :: Putter Int
putI16 = putter 2 DV.setInt16be
putI32 :: Putter Int
putI32 = putter 4 DV.setInt32be
putU8 :: Putter UInt
putU8 = putter 1 DV.setUint8
putU16 :: Putter UInt
putU16 = putter 2 DV.setUint16be
putU32 :: Putter UInt
putU32 = putter 4 DV.setUint32be
putF32 :: Putter Number
putF32 = putter 4 DV.setFloat32be
putF64 :: Putter Number
putF64 = putter 8 DV.setFloat64be

mapDataView :: forall e. ByteLength -> PutState -> Eff (arrayBuffer :: ARRAY_BUFFER | e) (Maybe DataView)
mapDataView n (PutState s) = do
  pure $ DV.slice s.off n $ DV.buffer s.dv

open :: forall e. ByteLength -> Eff (arrayBuffer :: ARRAY_BUFFER | e) PutState
open l = do
  ab <- AB.create l
  pure $ PutState { dv : DV.whole ab, off : 0 }

close :: PutState -> Eff (arrayBuffer :: ARRAY_BUFFER) ArrayBuffer
close (PutState s) = AB.slice 0 s.off (DV.buffer s.dv)

put :: ByteLength -> (PutState -> Eff (arrayBuffer :: ARRAY_BUFFER) PutState) -> Eff (arrayBuffer :: ARRAY_BUFFER) ArrayBuffer
put n f = open n >>= f >>= close

