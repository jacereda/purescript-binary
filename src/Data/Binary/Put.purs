module Data.Binary.Put where

import Prelude
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, DataView)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Data.Binary.Advancer (Advancer, advance)

type PutState = Advancer
type Putter a = a -> PutState -> Eff (writer :: DV.WRITER) PutState

putter :: forall a. Int -> DV.Setter a -> Putter a
putter n f v s = do
  o <- advance n s
  f s.dv v o
  pure s

putI8 :: Putter Int
putI8 = putter 1 DV.setInt8
putI16 :: Putter Int
putI16 = putter 2 DV.setInt16
putI32 :: Putter Int
putI32 = putter 4 DV.setInt32
putU8 :: Putter Int
putU8 = putter 1 DV.setUint8
putU16 :: Putter Int
putU16 = putter 2 DV.setUint16
putU32 :: Putter Int
putU32 = putter 4 DV.setUint32
putF32 :: Putter Number
putF32 = putter 4 DV.setFloat32
putF64 :: Putter Number
putF64 = putter 8 DV.setFloat64

mapDataView :: ByteLength -> PutState -> Eff (writer :: DV.WRITER) (Maybe DataView)
mapDataView n s = do
  o <- advance n s
  pure $ DV.slice o n (DV.buffer s.dv)

open :: ByteLength -> Eff (writer :: DV.WRITER) PutState
open l = pure $ { dv : DV.whole $ AB.create l, off : 0 }

close :: PutState -> Eff (writer :: DV.WRITER) ArrayBuffer
close s = pure $ AB.slice 0 s.off (DV.buffer s.dv)

put :: ByteLength -> (PutState -> Eff (writer :: DV.WRITER) PutState) -> ArrayBuffer
put n f = runWPure (open n >>= f >>= close)

foreign import runWPure :: forall e a. Eff (|e) a -> a
