module Examples.Ajax where

import Prelude
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import Data.Maybe (Maybe(..), maybe)
import Dispatcher.React (createLifecycleComponent, didMount, getProps, modifyState)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (get)
import React (ReactElement, createFactory)
import React.DOM (div', h1', img, text)
import React.DOM.Props (src)

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
