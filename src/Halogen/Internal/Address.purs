module Halogen.Internal.Address
  ( Address()
  , mkAddress
  , runAddress
  , cmapAddress
  , Send()
  , send
  , runSend
  , nothing
  ) where

import Prelude
import Control.Monad.Aff (Aff())
import Data.Functor.Contravariant (Contravariant, cmap)
import Data.NaturalTransformation (Natural())
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

import Halogen.Query (Action())
import Halogen.Effects (HalogenEffects())

data AddressF f eff = AddressF (Natural f (Aff (HalogenEffects eff)))

foreign import data Address :: (* -> *) -> *

mkAddress :: forall f eff. Natural f (Aff (HalogenEffects eff)) -> Address f
mkAddress driver = unsafeCoerce (AddressF driver)

runAddress :: forall f r. (forall eff. Natural f (Aff (HalogenEffects eff)) -> r) -> Address f -> r
runAddress k a = case unsafeCoerce a of
  AddressF driver -> k driver

cmapAddress :: forall g f. Natural g f -> Address f -> Address g
cmapAddress f = runAddress \driver -> mkAddress (driver <<< f)

data SendF f = SendF (Action f) (Maybe (Address f))

foreign import data Send :: *

send :: forall f. Address f -> Action f -> Send
send address action = unsafeCoerce (SendF action (Just address))

runSend :: forall r. (forall f. Action f -> Maybe (Address f) -> r) -> Send -> r
runSend k s = case unsafeCoerce s of
  SendF action address -> k action address

nothing :: Send
nothing = unsafeCoerce (SendF Just Nothing)
