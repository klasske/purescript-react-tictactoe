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

fromEffToString :: forall eff. Maybe Player -> Eff eff String
fromEffToString (Just p) = pure $ showPlayer p
fromEffToString Nothing = pure ""

square :: ReactClass SquareState
square = createClass squareSpec
  where
  squareSpec = (spec initialSquareState render)
    { displayName = "test" }
  
  toString (SquareState {valueFunction}) = do
    fromEffToString =<< valueFunction unit  

  setValue (SquareState { click }) = click unit
  render ctx = do
    props <- getProps ctx
    textValue <- toString props
    pure $ D.button 
            [ P.className "square" 
            , P.onClick (\_ -> 
                getProps ctx >>= setValue)
            ] 
            [ D.text textValue ]
    
        