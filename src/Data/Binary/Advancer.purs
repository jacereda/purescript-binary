module Data.Binary.Advancer where

import Data.ArrayBuffer.Types
import Control.Monad.Eff(Eff)

type Advancer = { dv :: DataView, off :: ByteOffset }

foreign import advance :: forall e. ByteOffset -> Advancer -> Eff ( |e) ByteOffset
