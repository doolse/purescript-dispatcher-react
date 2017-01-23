# purescript-dispatcher-react

A simple action dispatcher for `purescript-react`.

`bower install purescript-dispatcher-react --save`

The motivation behind this library are given in a [comparison](comparison.md) with some existing libraries. In summary:

* Facilitate writing React components which are black boxes.
* Make it easy to use purescript wrapper libraries which model React properties as purescript
  records which directly correspond to what the library expects. In particular many React libraries have event callbacks
  which are best modeled as `EffFnX` purescript functions.
* Always try and keep the code you write "type inferable". Being forced to enter a long type signature is never fun.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-dispatcher-react).

## Examples

Simple [toggle button](test/Examples/Toggler.purs) example:

```purescript
data Action = ToggleState

toggler :: ReactElement
toggler = createFactory (createComponent { on: false } render (effEval eval)) unit
  where

  render state (DispatchEff d) =
    div'
      [ h1'
          [ text "Toggle Button" ]
      , button
          [ onClick $ d \_ -> ToggleState ]
          [ text (if state.on then "On" else "Off") ]
      ]
  eval ToggleState = modifyState (\state -> { on: not state.on })
```

[Ajax example](test/Examples/AjaxExample.purs)

```purescript
data Action = FetchMovie

newtype Movie = Movie { title :: String, poster :: String}
instance movieIsForeign :: IsForeign Movie where
  read value = do
    title <- readProp "Title" value
    poster <- readProp "Poster" value
    pure $ Movie {title,poster}

type State = {movie::Maybe Movie}

ajax :: {title::String} -> ReactElement
ajax = createFactory $ createLifecycleComponent (didMount FetchMovie) {movie:Nothing} render eval
  where
    render {movie} = div' $ maybe nomovie showMovie movie
      where
      showMovie (Movie {title,poster}) = [ h1' [ text title ], img [src poster] [] ]
      nomovie = [ h1' [ text "Waiting for movie response" ] ]

    eval FetchMovie = do
      {title} <- getProps
      s <- lift $ get $ "http://www.omdbapi.com/?t=" <> encodeURIComponent title <> "&y=&plot=short&r=json"
      let movie = either (const $ Nothing) Just $ runExcept $ readJSON s.response
      modifyState _ {movie=movie}
```

[Lifecycle example](test/Examples/Lifecycle.purs)

```purescript
data Action = Init | Destroy

lifecycleComponent :: forall eff. {onClick :: Unit -> Eff eff Unit} -> ReactElement
lifecycleComponent = createFactory (createLifecycleComponent (didMount Init *> willUnmount Destroy) {} render eval) where
  render s (ReactProps p) (DispatchEff d) = div' [
        h1' [ text "I am alive" ]
      , button [onClick $ d \_ -> p.onClick unit] [ text "Click to kill me"]
    ]
  eval Init = log "I am alive"
  eval Destroy = log "I am dead"


lifecycleParent :: ReactElement
lifecycleParent = createFactory (createComponent {show:true} render unit) unit where
  render {show} (DispatchEff d) =
    if show
    then lifecycleComponent {onClick: d $ effEval \_ ->  modifyState _ {show=false}}
    else div' [ text "You killed it. Check the console" ]
```

[React native movie example](https://github.com/doolse/purescript-reactnative-example)
