module Component.Task where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Model

-- | The task component query algebra.
data TaskQuery a
  = UpdateDescription String a
  | ToggleCompleted Boolean a
  | IsCompleted (Boolean -> a)

type TaskProps =
  { onRemove :: Boolean -> Send
  , onToggle :: Boolean -> Send
  }

-- | The task component definition.
task :: forall g. (Functor g) => TaskProps -> Component Task TaskQuery g
task props = component render eval
  where

  render :: Address TaskQuery -> Task -> ComponentHTML
  render here t =
    H.li_ [ H.input [ P.inputType P.InputCheckbox
                    , P.title "Mark as completed"
                    , P.checked t.completed
                    , E.onChecked (pure <<< props.onToggle)
                    ]
          , H.input [ P.inputType P.InputText
                    , P.placeholder "Task description"
                    , P.value t.description
                    , E.onValueChange (E.input here UpdateDescription)
                    ]
          , H.button [ P.title "Remove task"
                     , E.onClick \_ -> pure (props.onRemove t.completed)
                     ]
                     [ H.text "✖" ]
          ]

  eval :: Natural TaskQuery (ComponentDSL Task TaskQuery g)
  eval (UpdateDescription desc next) = do
    modify (_ { description = desc })
    pure next
  eval (ToggleCompleted b next) = do
    modify (_ { completed = b })
    pure next
  eval (IsCompleted continue) = do
    b <- gets (_.completed)
    pure (continue b)
