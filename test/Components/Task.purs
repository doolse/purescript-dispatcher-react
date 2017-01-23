module Components.Task where

import Prelude
import React.DOM as R
import React.DOM.Props as RP
import Control.Monad.Eff (Eff)
import React (ReactClass, ReactElement, createElement)
import Dispatcher.React (createComponent, emptyHandler, ReactProps(..))
import Dispatcher (DispatchEff(..))
import Unsafe.Coerce (unsafeCoerce)

type TProps eff = { onChangeCompleted :: Boolean -> Eff eff Unit, onRemove :: Unit -> Eff eff Unit, task :: Task }

-- | The state for the task component
type Task =
  { completed :: Boolean
    , description :: String
  }

initialTask :: String -> Task
initialTask s = { completed: false, description: s }

task :: forall eff. (TProps eff -> TProps eff) -> Task -> ReactElement
task p t = createElement taskClass (p {task:t, onRemove: emptyHandler, onChangeCompleted: emptyHandler}) []

taskClass :: forall eff. ReactClass (TProps eff)
taskClass = createComponent unit render unit
  where
  render _ (ReactProps {task:s, onRemove, onChangeCompleted}) (DispatchEff d) =
    R.tr' <<< map (R.td' <<< pure) $
        [ R.input [ RP._type "checkbox"
                  , RP.className "checkbox"
                  , RP.checked s.completed
                  , RP.title "Mark as completed"
                  , RP.onChange $ d $ \e -> onChangeCompleted (unsafeCoerce e).target.checked
                  ] []
        , R.text s.description
        , R.a [ RP.className "btn btn-danger pull-right"
              , RP.title "Remove item"
              , RP.onClick $ d $ \_ -> onRemove unit
              ]
              [ R.text "✖" ]
        ]
