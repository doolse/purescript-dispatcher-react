# What is a UI component?

```purescript
renderComponent :: State -> Markup
```

The most basic description of a component is just a pure function which takes some state and creates markup for it.
Usually within this State is some sort of way for the component to communicate back to the caller (the "parent" component).
In the case of the DOM and React these are usually event handler functions, for example a cut down HTML Button element
could be represented as:

```purescript
type ButtonState = {text :: String, onClick :: Event -> Handled}
button :: ButtonState -> HTMLElement
```

To use this component the parent only needs to supply an `onClick` handler. Simple. In fact it's pretty hard to get away
from this style of component if you're working with the web, after all the DOM uses this style.

#### Aside - event handlers

I've used a made up type `Handled` here to really represent "child component doesn't care". In the case of the DOM the purescript type for
event handlers is actually `forall eff. EffFn1 eff Event Unit` as they expect the effects to have already been run before the function returns.

## Private state

Obviously if that was all there was to a component we'd simply call them functions and be done with it! But in reality components
also want to modify their own state (or at least part of it).

Let's take the example of a button which has to be clicked a certain number
of times before the parent is notified. It is still possible to make a single function which can handle this:

```purescript
type CountPrivate = { clicksLeft :: Int }
type CountPublic = { clicks :: Int, onFinishedClicking :: Unit -> Handled }

countButton :: {privateState :: CountPrivate, updatePrivateState :: (CountPrivate -> CountPrivate) -> Handled} -> CountPublic -> HTMLElement
```

So provided the parent keeps a `PrivateState` and can give the component a way to update it, you can write this component.
However there is still a problem initializing the `PrivateState`, as you still need a function to initialize it from the `CountPublic`.

```purescript
initializePrivateState :: CountPublic -> CountPrivate
```

The only problem for the parent now is that it is also responsible for knowing when the state needs to be initialized.

What I've just described is the conceptial basis of how React, [Halogen](https://github.com/slamdata/purescript-halogen) and [Thermite](https://github.com/paf31/purescript-thermite) (on top of React) all work. Each have slightly different ways of how to keep track of the state and how to write event handlers. I will start with React and contrast that
with Thermite and Halogen and explain why I was motivated to write this library.

## React

Why does the parent need to know anything about `CountPrivate`? Shouldn't that state be private? In my opinion yes.

This is the approach that React takes. The terminology which React uses is `Props` for `CountPublic` and `State` for `CountPrivate`. The `initializePrivateState`
function is hidden away in a `ReactClass` which is only parametrized on the `Props` type.
Let's make a `purescript-react` version of the countButton. (Only type signatures required for it to compile are added)

```purescript
type CountButtonProps eff = { clicks::Int, onFinishedClicks :: Unit -> Eff eff Unit }

countButton :: forall eff. CountButtonProps eff -> ReactElement
countButton = createFactory countButtonClass
  where
  -- countButtonClass :: ReactClass (CountButtonProps eff)
  countButtonClass = createClass (spec' getInitialState render)
    where
      getInitialState this = (\p -> {clicksLeft: p.clicks}) <$> getProps this

      render this = do
        p <- getProps this
        s <- readState this
        let clicked = if s.clicksLeft == 1 then
                unsafeCoerceEff $ p.onFinishedClicks unit
              else
                transformState this \st -> st {clicksLeft = st.clicksLeft - 1}
        pure $ button [onClick \_ -> clicked]
                      [text $ "Click me " <> show s.clicksLeft <> " more times"]
```

The real problem with this code is that the render function is in the `Eff` monad which is really against the spirit of what it's doing, it's meant to be a
pure function from state to markup. This is due to the peculiarities of having to work with the Object oriented React API.

Ignoring this ugliness, the main thing to take away from this is that the state is private, freeing the parent component
from the burdon of having to keep track of it, which makes it very easy to use. Hidden inside `ReactElement` is
the smarts to know if it's the first time a component has been rendered (getInitialState will be called) or if it has already been
rendered and already has a state. See [Reconciliation](https://facebook.github.io/react/docs/reconciliation.html) for full details.

## Thermite

`purescript-thermite` is a thin wrapper around `purescript-react`, however it really ignores props and has it's own
 philosophy on combining components. First of all it allows you to write a pure render method:

 ```purescript
 type Render state props action
    = (action -> EventHandler)
   -> props
   -> state
   -> Array React.ReactElement
   -> Array React.ReactElement
```

The first argument is a "dispatcher", really just a way of separating the state modification and effectful code away from
the rendering by having all actions be represented in the `action` type. Then component "spec" has a function which turns these actions into effects.

Thermite version of this example:

```purescript
data CountButtonAction = Clicked (CountButtonAction -> EventHandler) | FinishedClicking
type CountButtonState = { clicksLeft::Int }

countButtonInitialState :: Int -> CountButtonState
countButtonInitialState clicksLeft = {clicksLeft}

countButtonSpec :: forall eff. Spec eff CountButtonState Unit CountButtonAction
countButtonSpec = simpleSpec performAction render
  where
    performAction FinishedClicking _ _ = pure unit
    performAction (Clicked dispatch) _ {clicksLeft} =
      if clicksLeft == 1
        then lift $ unsafeCoerceAff $ liftEff $ dispatch FinishedClicking
        else void $ modifyState \s -> s {clicksLeft=s.clicksLeft - 1}

    render :: forall props. Render CountButtonState props CountButtonAction
    render dispatch _ s _ = [
      button [onClick \_ -> dispatch $ Clicked dispatch]
        [text $ "Click me " <> show s.clicksLeft <> " more times"]
    ]
```

So we now have a pure render method (unfortunately the type can't be correct inferred so the type signature was necessary) we also have a nicer
`CountButtonAction` type and `performAction` function for handling the event. However:

* There is an explicit `countButtonInitialState` function which hints at the parent having to track the child state.
* The `onFinishedClicking` event has now become an action of this component which is just handled by doing nothing. The only way the parent can
  handle this event is by "peeking" at the action.
* The `Clicked` action has to pass through the dispatcher because the "peeking" mechanism only works if you dispatch an action through it, you can't
  just call `performAction` again or the parent won't see it.


## Halogen

Halogen is another UI library which isn't built on top of React but instead use VirtualDOM directly, essentially a React replacement.

```purescript
type CountButtonState = { clicksLeft :: Int }

countButtonInitialState :: Int -> CountButtonState
countButtonInitialState clicksLeft = { clicksLeft  }

data CountButtonQuery a = Clicked a | IsFinishedClicking (Boolean -> a)

countButton :: forall g. H.Component CountButtonState CountButtonQuery g
countButton = H.component { render, eval }
  where
  -- render :: CountButtonState -> H.ComponentHTML CountButtonQuery
  render state = HH.button [HE.onClick (HE.input_ Clicked)]
    [HH.text $ "Click me " <> show state.clicksLeft <> " more times"]

  eval :: CountButtonQuery ~> H.ComponentDSL CountButtonState CountButtonQuery g
  eval (IsFinishedClicking f) = f <<< (eq 1) <$> H.gets _.clicksLeft
  eval (Clicked next) = do
    clicksLeft <- H.gets _.clicksLeft
    if clicksLeft > 1 then H.modify (\state -> { clicksLeft: state.clicksLeft - 1 }) else pure unit
    pure next
```

Once again pure render method and `action` separation, however it's slightly different:

* The markup you render is typed on the `Query` type and the HTML abstract only allows
  events of this type to be dispatched. No arbitrary effects allowed.
* Rather than just an action, you define a "Query algebra" which allows you to return values
  to the caller.
* Once again parent communication is by "peeking" only. However the situation is worse
  in this particular case as there is no way to send another `Query` inside the `eval` function, so
  the parent will have to know that it needs to query `IsFinishedClicking` when it peeks
  the `Clicked` event.

## Parent components

So we've defined our component, how do we use it? Let's start with pure React.

```purescript
useCountButton :: {clicks1 :: Int, clicks2 :: Int } -> ReactElement
useCountButton = createFactory $ createClass $ spec unit render
  where
    render this = do
      {clicks1,clicks2} <- getProps this
      pure $ div' [
          countButton {clicks:clicks1, onFinishedClicks: \_ -> logClick "Button 1" clicks1}
        , countButton {clicks:clicks2, onFinishedClicks: \_ -> logClick "Button 2" clicks2}
      ]
    logClick name count = log $ name <> " was clicked " <> show count <> " times."
```

This is a stateless component, which is actually pretty pointless as you're better off just
making it a function:

```purescript
useCountButton' :: {clicks1 :: Int, clicks2 :: Int } -> ReactElement
useCountButton' {clicks1,clicks2} =
  div' [
      countButton {clicks:clicks1, onFinishedClicks: \_ -> logClick "Button 1" clicks1}
    , countButton {clicks:clicks2, onFinishedClicks: \_ -> logClick "Button 2" clicks2}
  ]
  where
    logClick name count = log $ name <> " was clicked " <> show count <> " times."
```

### Thermite parent

```purescript
type ParentState = {
    clicks1 :: CountButtonState
  , clicks2 :: CountButtonState
  , initialClicks1 :: Int
  , initialClicks2 :: Int
}

_clicks1 :: Lens' ParentState CountButtonState
_clicks1 = lens _.clicks1 _ {clicks1=_}

_clicks2 :: Lens' ParentState CountButtonState
_clicks2 = lens _.clicks2 _ {clicks2=_}

_clicksPrism :: Int -> (ParentState -> Int) -> Prism' ParentAction CountButtonAction
_clicksPrism n f = prism' (ButtonAction n f) \pa -> case pa of
  (ButtonAction c _ ba) | c == n -> Just ba
  _ -> Nothing

data ParentAction = ButtonAction Int (ParentState -> Int)  CountButtonAction

initialUseButtonState :: Int -> Int -> ParentState
initialUseButtonState initialClicks1 initialClicks2 = { initialClicks1, initialClicks2
  , clicks1: countButtonInitialState initialClicks1 , clicks2: countButtonInitialState initialClicks2 }

useCountButtonSpec :: forall eff. Spec (console::CONSOLE|eff) ParentState Unit ParentAction
useCountButtonSpec = fold [
    focus _clicks1 (_clicksPrism 1 _.initialClicks1) countButtonSpec
  , focus _clicks2 (_clicksPrism 2 _.initialClicks2) countButtonSpec
  , simpleSpec performAction defaultRender
  ]
  where
    performAction (ButtonAction num initial FinishedClicking) _ s =
      lift $ log $ "Button " <> show num <> " was clicked " <> show (initial s) <> " times."
    performAction _ _ _ = pure unit
```

Ok this behaves exactly the same as the React parent component but note:

* The parent must keep track of the child state explicitly.
* Combining components is achieved by having child components `focus` in on their state and actions with a `Lens` and `Prism`.
  These focussed specs can then be combined with the spec's `Monoid` instance. You have to use a `Lens` on the child component spec to
  wrap it's render method if you need anything more compicated than rendering them one after the other.
* Your `peformAction` method will usually end up with a catch-all case which isn't great when you add new actions as you lose the compiler all cases covered check.
* Plenty of Boilerplate.

### Halogen parent

```
type UseCountSlot = Int
type UseState = {clicks1 :: Int, clicks2 :: Int}
type UseState' eff = ParentState UseState CountButtonState Identity CountButtonQuery (Aff eff) UseCountSlot
type UseQuery' = Coproduct Identity (ChildF UseCountSlot CountButtonQuery)

useButtonInitialState :: forall eff. Int -> Int -> UseState' eff
useButtonInitialState clicks1 clicks2 = parentState {clicks1, clicks2}
useButton :: forall eff. H.Component (UseState' (console::CONSOLE|eff)) UseQuery' (Aff (console::CONSOLE|eff))
useButton = parentComponent {render,eval,peek: Just peek}
  where
    render state = HH.div_ [
      HH.slot 1 \_ -> { component: countButton, initialState: countButtonInitialState state.clicks1 }
    , HH.slot 2 \_ -> { component: countButton, initialState: countButtonInitialState state.clicks2 }
    ]
    eval :: Identity ~> H.ParentDSL UseState CountButtonState Identity CountButtonQuery (Aff (console::CONSOLE|eff)) UseCountSlot
    eval (Identity i) = pure i

    peek (ChildF p q) = case q of
      Clicked _ -> do
        finished <- query p (H.request IsFinishedClicking)
        if fromMaybe false finished then do
            s <- H.get
            fromEff $ log $ "Button " <> show p <> " was clicked " <> show (case p of
              1 -> s.clicks1
              _ -> s.clicks2) <> " times."
          else pure unit
        pure unit
      _ -> pure unit
```

* Halogen takes the approach of having a different type for parent components, with an explicit type
  for the child component type. You don't have to explicitly manage each child state but rather
  the states are stored as a `Map` with the key being a particular "Slot" type. The `slot` function
  takes one of the keys and a function which initialises the state if the key does not exist in the map yet.
* Peeking is done with an optional peek function which is where the parent must query the child for `IsFinishedClicking`.
* The `eval` function requires an explicit type signature to compile.
* Because the child type is part of the Parent components signature, things start to get messy and boilerplatey when you have
  children of [different types](https://github.com/slamdata/purescript-halogen/blob/master/GUIDE.md#multiple-types-of-child-component).

## Dispatcher

Obviously of the 3 approaches I prefer the React one for it's simplicity but you don't want to write everything in `Eff`, so I made this library.

```purescript
type CountButtonProps eff = { clicks::Int, onFinishedClicks :: Unit -> Eff eff Unit }

data Action = Clicked

countButton :: forall eff. CountButtonProps eff -> ReactElement
countButton = createFactory $ createComponent' getInitialState render (effEval eval)
  where
    getInitialState (ReactProps p) = ReactState {clicksLeft: p.clicks}
    render s (DispatchEff d) = button [onClick $ d \_ -> Clicked]
                                      [text $ "Click me " <> show s.clicksLeft <> " more times"]
    eval Clicked = do
      {clicksLeft} <- getState
      if clicksLeft == 1
       then do
        {onFinishedClicks} <- getProps
        execHandler $ onFinishedClicks unit
       else modifyState \s -> s {clicksLeft = s.clicksLeft - 1}
```

Essentially the same as the React implementation but `render` and `getInitialState` are no longer in `Eff`.

The important thing to note here is the use of the newtype `DispatchEff` in the render function.

```purescript
newtype DispatchEff action = DispatchEff (forall event eff. (event -> action) -> event -> Eff eff Unit)
```

The `purescript-react` library has DOM functions which take event handlers in the shape of:

`forall eff. event -> Eff eff Unit`

Which is exactly what `DispatchEff` is made for, given a function which takes an event and returns an action, i will give
you a function which takes an event and returns a `forall eff. Eff eff unit`. Some other libraries use different signatures for their
handlers, for example in `purescript-reactnative` i took the approach of encoding them in purescript as `EffFnX` functions (see [`purescript-eff-functions`](https://github.com/hdgarrood/purescript-eff-functions)). So there are newtypes for those too:

```purescript
newtype DispatchEffFn action = DispatchEffFn (forall event eff. (event -> action) -> EffFn1 eff event Unit)
newtype DispatchEffFn2 action = DispatchEffFn2 (forall event ev2 eff. (event -> ev2 -> action) -> EffFn2 eff event ev2 Unit)
newtype DispatchEffFn3 action = DispatchEffFn3 (forall event ev2 ev3 eff. (event -> ev2 -> ev3 -> action) -> EffFn3 eff event ev2 ev3 Unit)
```

You can make a higher arrity `EffFnX` by using `DispatchEff` if needs be.

```purescript
mkEffFn4 \ev1 ev2 ev3 -> d \ev4 -> action
```

The first argument of the `render` function is always the state, however the rest of the arguments can be anything that has an instance
of the `FromContext` type class.

```purescript
render s (DispatchEff d) (DispatchEffFn d1) (ReactProps p) (ReactChildren c) = ...
```

So there are newtype's for getting the React props and Children, and you can also combine as many Dispatcher's as your child components need.
