-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HTML()
  , Prop()
  , module Halogen.Component
  , module Halogen.Driver
  , module Halogen.Effects
  , module Halogen.Query
  , module Halogen.Internal.Address
  , module Data.NaturalTransformation
  ) where

import Prelude

import Data.NaturalTransformation (Natural())

import Halogen.Component
import Halogen.Driver
import Halogen.Effects
import Halogen.Query
import Halogen.Internal.Address (Address(), Send(), send, nothing)
import Halogen.HTML.Core as C

-- | A specialised version of the `Halogen.HTML.Core.HTML` type where `i` is
-- | fixed to the `Send` type
type HTML p = C.HTML p Send

-- | A specialised version of the `Halogen.HTML.Core.Prop` type where `i` is
-- | fixed to the `Send` type
type Prop = C.Prop Send
