module Square where

import Prelude
import Player (Player, showPlayer)
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import React (ReactClass, ReactState, ReadWrite, ReadOnly, createClass, spec, getProps)


newtype SquareState = SquareState
    { valueFunction :: forall t1. Unit -> Eff (console:: CONSOLE, state :: ReactState ReadOnly | t1) (Maybe Player)
    , click :: forall t1. Unit -> Eff (console:: CONSOLE, state :: ReactState ReadWrite | t1) Unit
    }

initialSquareState :: SquareState
initialSquareState = 
  SquareState
    { valueFunction: \_ -> pure Nothing
    , click: \_ -> pure unit
    }

toString :: forall t1. SquareState -> Eff (console:: CONSOLE, state :: ReactState ReadOnly | t1) String
toString (SquareState {valueFunction}) = do
    player <- valueFunction unit  
    case player of
        Just p -> pure $ showPlayer p
        Nothing -> pure $ "" 

setValue ::  forall t1. SquareState -> Eff (console:: CONSOLE, state :: ReactState ReadWrite | t1) Unit
setValue (SquareState { click }) = click unit

square :: ReactClass SquareState
square = createClass squareSpec
  where
  squareSpec = (spec initialSquareState render)
    { displayName = "square" }  

  render ctx = do
    props <- getProps ctx
    textValue <- toString props
    pure $ D.button 
            [ P.className "square" 
            , P.onClick \_ -> getProps ctx >>= setValue
            ] 
            [ D.text textValue ]
    
        