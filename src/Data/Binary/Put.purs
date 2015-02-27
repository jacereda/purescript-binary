module Data.Binary.Put where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import Data.Function
import Data.Maybe
import Control.Monad.Eff
import Data.Binary.Advancer

type PutState = Advancer
type Putter a = a -> PutState -> Eff (writer :: DV.Writer) PutState

putter :: forall a. Number -> DV.Setter a -> Putter a
putter n f v s = do
  o <- advance n s
  f s.dv v o
  return s

putI8 :: Putter Number
putI8 = putter 1 DV.setInt8
putI16 :: Putter Number
putI16 = putter 2 DV.setInt16
putI32 :: Putter Number
putI32 = putter 4 DV.setInt32
putU8 :: Putter Number
putU8 = putter 1 DV.setUint8
putU16 :: Putter Number
putU16 = putter 2 DV.setUint16
putU32 :: Putter Number
putU32 = putter 4 DV.setUint32
putF32 :: Putter Number
putF32 = putter 4 DV.setFloat32
putF64 :: Putter Number
putF64 = putter 8 DV.setFloat64

mapDataView :: ByteLength -> PutState -> Eff (writer :: DV.Writer) (Maybe DataView)
mapDataView n s = do
  o <- advance n s
  return $ DV.slice o n (DV.buffer s.dv)

open :: ByteLength -> Eff (writer :: DV.Writer) PutState
open l = return $ { dv : DV.whole $ AB.create l, off : 0 }

close :: PutState -> Eff (writer :: DV.Writer) ArrayBuffer
close s = return $ AB.slice 0 s.off (DV.buffer s.dv)

put :: ByteLength -> (PutState -> Eff (writer :: DV.Writer) PutState) -> ArrayBuffer
put n f = runWPure (open n >>= f >>= close)

foreign import runWPure
"""
function runWPure(f) {
  return f();
}
""" :: forall e a. Eff (|e) a -> a
