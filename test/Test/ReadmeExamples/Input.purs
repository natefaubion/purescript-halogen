module Test.ReadmeExamples.Input where

import Prelude
import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

data ExampleQuery a
  = SetOption Boolean a
  | SetText String a

input1 :: Address ExampleQuery -> P.IProp (checked :: P.I, onChange :: P.I) Send
input1 here = E.onChecked (E.input here SetOption)

input2 :: Address ExampleQuery -> P.IProp (value :: P.I, onInput :: P.I) Send
input2 here = E.onValueInput (E.input here SetText)
