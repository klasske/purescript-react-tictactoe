module Game where

import Prelude
import Board (board, emptyBoardState)
import React.DOM as D
import React.DOM.Props as P
import React (ReactClass, ReactThis, ReactElement, createFactory, createClass, spec, readState, transformState)

newtype GameState = GameState { boardSize :: Int }

initialState :: GameState
initialState = GameState { boardSize: 3  }

getBoardSize :: GameState -> Int 
getBoardSize ( GameState { boardSize } ) = boardSize

addX :: GameState -> Int -> GameState
addX ( GameState { boardSize } ) x = 
    if boardSize + x > 1 && boardSize + x < 20 then  
        GameState {boardSize: boardSize + x}
    else GameState {boardSize: boardSize}

addButton :: forall state. ReactThis state GameState -> Int -> String -> ReactElement
addButton ctx addValue icon =
    D.button 
        [ P.onClick \_ -> transformState ctx \state -> addX state addValue
        , P.className "btn size-btn"]
        [D.i [P.className $ "fa fa-" <> icon ] []]

game :: forall props. ReactClass props
game = createClass gameSpec
  where
  gameSpec = (spec initialState render)
    { displayName = "game" }
  render ctx = do
    state' <- readState ctx 
    pure $ D.div[ P.className "game-container"]
                [ D.div [ P.className "board-size"] 
                        [ addButton ctx 1 "plus"   
                        , D.div [P.className "square"] [D.text (show $ getBoardSize state')]
                        , addButton ctx (-1) "minus"  
                        ]
                , D.div [ P.className "game"]
                        [ D.div [ P.className "game-board"] 
                                [ createFactory (board $ getBoardSize state') $ emptyBoardState (getBoardSize state')] ]
                ]

