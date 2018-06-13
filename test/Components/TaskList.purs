module Components.TaskList where

import Prelude

import Components.Task (Task, initialTask, task)
import Data.Array (fromFoldable)
import Data.Filter (Filter(..), showFilter)
import Data.List (List(..), deleteAt, filter, length, mapWithIndex, modifyAt)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(Tuple), snd)
import Dispatcher (action)
import Dispatcher.React (modifyState, renderer)
import React (ReactElement, component, unsafeCreateLeafElement)
import React.DOM (text, p', td', input, tr', tbody', th, thead', table, div, h1', button) as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)


-- | An action for the full task list component
data TaskListAction
  = NewTask String
  | SetEditText String
  | SetFilter Filter
  | RemoveTask Int
  | TaskChangeCompletion Int Boolean

-- | The state for the full task list component is a list of tasks
type TaskListState =
  { tasks       :: List Task
  , editText    :: String
  , filter      :: Filter
  }

initialTaskListState :: TaskListState
initialTaskListState =
  { tasks: Nil
  , editText: ""
  , filter: All
  }

taskList :: ReactElement
taskList = unsafeCreateLeafElement (component "TaskList" spec) {} 
  where
  spec this = let 
    performAction (RemoveTask i) = modifyState \state -> state { tasks = fromMaybe state.tasks (deleteAt i state.tasks) }
    performAction (SetEditText s) = modifyState (_ { editText = s })
    performAction (SetFilter f) = modifyState (_ { filter = f })
    performAction (NewTask s) = modifyState $ \state -> state { tasks = Cons (initialTask s) state.tasks }
    performAction (TaskChangeCompletion i b) = modifyState \state -> state { tasks = fromMaybe state.tasks (modifyAt i _ {completed=b} state.tasks) }
    d = action this <<< performAction
    md = action this <<< (maybe (pure unit) performAction)

    -- md is a dispatcher which take a `Maybe TaskListAction`
    taskListRender {state:s} = container $
        header <> [
          table s.tasks
        , footer
        ]
      where
        -- | A function which wraps a `Spec`'s `Render` function with a `container` element.
        container = R.div [ RP.className "container" ]

        -- The header component contains a button which will create a new task.
        header = [
                R.h1' [ R.text "todo list" ]
              , R.div [ RP.className "btn-group" ] (map filter_ [ All, Active, Completed ])
              ]
              where
              filter_ :: Filter -> ReactElement
              filter_ f = R.button [ RP.className (if f == s.filter then "btn toolbar active" else "btn toolbar")
                                  , RP.onClick $ \_ -> d $ SetFilter f
                                  ]
                                  [ R.text (showFilter f) ]
        matches All       _ = true
        matches Completed t = t.completed
        matches Active    t = not t.completed

        table tasks =
          let handleKeyPress :: Int -> String -> Maybe TaskListAction
              handleKeyPress 13 text = Just $ NewTask text
              handleKeyPress 27 _    = Just $ SetEditText ""
              handleKeyPress _  _    = Nothing
              renderTask (Tuple i t) = task _ {
                  onRemove = \_ -> d $ RemoveTask i
                , onChangeCompleted = \b -> d $ TaskChangeCompletion i b } t
          in R.table [ RP.className "table table-striped" ]
                      [ R.thead' $ [ R.tr' [ R.th [ RP.className "col-md-1"  ] []
                                            , R.th [ RP.className "col-md-10" ] [ R.text "Description" ]
                                            , R.th [ RP.className "col-md-1"  ] []
                                            ]
                                    ]
                      , R.tbody' $ [ R.tr' [ R.td' []
                                            , R.td' [ R.input [ RP.className "form-control"
                                                              , RP.placeholder "Create a new task"
                                                              , RP.value s.editText
                                                              , RP.onKeyUp $ \e -> md $ handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
                                                              , RP.onChange $ \e -> d $ SetEditText (unsafeCoerce e).target.value
                                                              ] 
                                                    ]
                                            , R.td' []
                                            ]
                                    ] <> (fromFoldable $ renderTask <$> (filter (snd >>> matches s.filter) $ mapWithIndex Tuple s.tasks))
                      ]


        -- It simply displays a label with information about completed tasks.
        footer =
          let
            footerText = show completed <> "/" <> show total <> " tasks completed."
            completed  = length $ filter _.completed s.tasks
            total      = length s.tasks
          in R.p' [ R.text footerText ]
    in pure {state: initialTaskListState, render: renderer taskListRender this}
