module Examples.Ajax where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Dispatcher (affAction)
import Dispatcher.React (getProps, modifyState, renderer)
import Global.Unsafe (unsafeEncodeURIComponent)
import Network.HTTP.Affjax (get)
import Network.HTTP.Affjax.Response (json)
import React (ReactElement, component, unsafeCreateLeafElement)
import React.DOM (div', h1', img, text)
import React.DOM.Props (src)

data Action = FetchMovie

newtype Movie = Movie { title :: String, poster :: String}
instance decodeMovie :: DecodeJson Movie where
  decodeJson v = do
    o <- decodeJson v
    title <- o .? "Title"
    poster <- o .? "Poster"
    pure $ Movie { title, poster }

type State = {movie::Maybe Movie}

ajax :: {title::String} -> ReactElement
ajax = unsafeCreateLeafElement $ component "Ajax" spec
  where
    spec this = 
      let d = affAction this <<< eval
          render {state:{movie}} = div' $ maybe nomovie showMovie movie
                  where
                  showMovie (Movie {title, poster}) = [ h1' [ text title ], img [src poster] ]
                  nomovie = [ h1' [ text "Waiting for movie response" ] ]
      in pure {state: {movie:Nothing} :: State, componentDidMount: d FetchMovie, render: renderer render this}

    eval FetchMovie = do
      {title} <- getProps
      s <- lift $ get json $ "http://www.omdbapi.com/?t=" <> unsafeEncodeURIComponent title <> "&y=&plot=short&r=json&apikey=16824787"
      let movie = (either (const $ Nothing) Just $ decodeJson s.response) 
      modifyState _ {movie=movie}
