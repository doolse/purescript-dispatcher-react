module Components.Task where

import Prelude

import Dispatcher.React (emptyHandler, propsRenderer)
import Effect (Effect)
import React (ReactClass, ReactElement, component, createLeafElement)
import React.DOM as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)

type TProps = { onChangeCompleted :: Boolean -> Effect Unit, onRemove :: Unit -> Effect Unit, task :: Task }

-- | The state for the task component
type Task =
  { completed :: Boolean
    , description :: String
  }

initialTask :: String -> Task
initialTask s = { completed: false, description: s }

task :: (TProps -> TProps) -> Task -> ReactElement
task p t = createLeafElement taskClass (p {task:t, onRemove: emptyHandler, onChangeCompleted: emptyHandler})

taskClass :: ReactClass TProps
taskClass = component "Task" spec
  where
  spec this = let 
    render {task:s, onRemove, onChangeCompleted} =
      R.tr' <<< map (R.td' <<< pure) $
          [ R.input [ RP._type "checkbox"
                    , RP.className "checkbox"
                    , RP.checked s.completed
                    , RP.title "Mark as completed"
                    , RP.onChange $ \e -> onChangeCompleted (unsafeCoerce e).target.checked
                    ] 
          , R.text s.description
          , R.a [ RP.className "btn btn-danger pull-right"
                , RP.title "Remove item"
                , RP.onClick $ \_ -> onRemove unit
                ]
                [ R.text "✖" ]
          ]
    in pure {render: propsRenderer render this}
