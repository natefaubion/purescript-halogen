module Test.ReadmeExamples.Handler where

import Prelude
import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Data.Functor (($>))
import qualified Halogen.HTML.Events.Handler as EH

data Query a = ToggleState a

handled :: Address Query -> P.IProp (onClick :: P.I) Send
handled here = E.onClick \_ -> EH.preventDefault $> send here ToggleState
