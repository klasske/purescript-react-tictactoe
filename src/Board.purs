module Board where

import Prelude

import Square (SquareState(..), square)
import Player (Player(..), flipPlayer, showPlayer, allOfPlayer)
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Control.Alt ((<|>))
import Data.Array ((:), (..), mapMaybe, head)
import Data.List (List, range, index, mapWithIndex)
import React (ReactClass, createFactory, createClass, spec, readState, writeStateWithCallback)

fromNest :: forall t1. Maybe (Maybe t1) -> Maybe t1
fromNest (Just c) = c
fromNest Nothing = Nothing 

newtype BoardState = BoardState 
    { squares :: List (Maybe Player)
    , isNext :: Player}

emptyBoardState :: Int -> BoardState
emptyBoardState boardSize = BoardState 
    { squares: range 0 (boardSize * boardSize - 1) # map (\_ -> Nothing) 
    , isNext: X}

winningLines :: Int -> Array (Array Int)
winningLines boardSize =
    diagonal1 : diagonal2 : append horizontal vertical
    where    
        baseRange = (0..(boardSize - 1))
        horizontal = baseRange # map (\row -> baseRange # map (\cell -> row * boardSize + cell ))
        vertical = baseRange # map (\row -> baseRange # map (\cell -> cell * boardSize + row ))
        diagonal1 = baseRange # map (\i -> i * boardSize + i )
        diagonal2 = baseRange # map (\i -> (boardSize - 1 - i) * boardSize + i )

isWinner :: List (Maybe Player) -> Array Int -> Maybe Player
isWinner squares = map (index squares >>> fromNest ) >>> (\y -> allOfPlayer X y <|> allOfPlayer O y)

board :: Int -> ReactClass BoardState
board boardSize = createClass boardSpec 
  where
  boardSpec = (spec (emptyBoardState boardSize) render)
    { displayName = "board"}

  winningLines' = winningLines boardSize  
  hasWinner squares = winningLines' # mapMaybe (isWinner squares) # head

  logWinner (BoardState {squares}) = 
    case hasWinner squares of
       Just player -> showPlayer player
       Nothing -> "No winner"

  gameHasEnded (BoardState {squares}) = isJust $ hasWinner squares
  gameStatus (BoardState {squares, isNext}) =     
    case hasWinner squares of
       Just player -> "Winner: " <> showPlayer player
       Nothing -> "Next player: " <> showPlayer isNext

  readSquare :: Int -> BoardState -> (Maybe Player)
  readSquare i (BoardState {squares}) = do
    index squares i # fromNest

  updateSquare i (BoardState {squares, isNext}) = do 
    BoardState 
        { squares : mapWithIndex (\j s -> if j == i then (Just isNext) else s) squares
        , isNext: flipPlayer isNext
        }

  validMove boardState i = (not $ gameHasEnded boardState) && (isNothing $ readSquare i boardState)

  rendersquare ctx i =                
    createFactory square 
      (SquareState 
        { valueFunction: \_ -> do
            rState <- readState ctx
            pure $ readSquare i rState
        , click: \_ -> do
            rState <- readState ctx
            writeStateWithCallback ctx (if (validMove rState i) then updateSquare i rState else rState) (pure unit) >>= logWinner >>> log
        })

  boardRow ctx row  =
    D.div [P.className "board-row"] 
        $ map (\i -> rendersquare ctx $ row * boardSize + i ) (0..(boardSize - 1))

  render ctx = do
    state' <- readState ctx
    pure $ D.div [] $ 
        D.div [P.className "status"] [D.text (gameStatus state')] : map (boardRow ctx) (0..(boardSize - 1))
       