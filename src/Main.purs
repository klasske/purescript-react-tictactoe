module Main where

import Prelude
import React.DOM as D
import React.DOM.Props as P
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
-- import Data.Int (decimal, toStringAs)

import Data.Maybe (Maybe(..), fromJust)
import Data.List (List, range, index, foldl, mapWithIndex)
import Data.Array ((:), (..), mapMaybe, head)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, ReactClass, ReactState, ReadWrite, ReadOnly, createFactory, createClass, spec, getProps, readState, writeStateWithCallback)
import ReactDOM (render)

foreign import interval :: forall eff a.
                             Int ->
                             Eff eff a ->
                             Eff eff Unit


boardSize :: Int
boardSize = 3

winningLines :: Array (Array Int)
winningLines =
    diagonal1 : diagonal2 : append horizontal vertical
    where    
        baseRange = (0..(boardSize - 1))
        horizontal = baseRange # map (\row -> baseRange # map (\cell -> row * boardSize + cell ))
        vertical = baseRange # map (\row -> baseRange # map (\cell -> cell * boardSize + row ))
        diagonal1 = baseRange # map (\i -> i * boardSize + i )
        diagonal2 = baseRange # map (\i -> (boardSize - 1 - i) * boardSize + i )


newtype GameState = GameState
  { player :: String }
newtype BoardState = BoardState 
    { squares :: List String
    , xIsNext :: Boolean}
newtype SquareState = SquareState
    { valueFunction :: forall t1. Unit -> Eff (console:: CONSOLE, state :: ReactState ReadOnly | t1) (Maybe String)
    , click :: forall t1. Unit -> Eff (console:: CONSOLE, state :: ReactState ReadWrite | t1) Unit
    , test :: String
    }

initialState :: GameState
initialState = GameState { player: "X"  }
initialSquareState :: SquareState
initialSquareState = 
  SquareState
    { valueFunction: \_ -> pure Nothing
    , click: \_ -> pure "Why doesn't this update?" >>= log
    , test: "?"
    }
initialBoardState :: BoardState
initialBoardState = BoardState 
    { squares: range 0 (boardSize * boardSize - 1) # map (\_ -> "") 
    , xIsNext: true}



square :: ReactClass SquareState
square = createClass squareSpec
  where
  squareSpec = (spec initialSquareState render)
    { displayName = "test" }
  fromEffToString :: forall eff. Maybe String -> Eff eff String
  fromEffToString test =
    case test of
     Just x -> pure x
     Nothing -> pure "-"
  toString (SquareState {valueFunction}) = do
    fromEffToString =<< valueFunction unit  

  squareid (SquareState { test }) = "square-" <> test
  setValue (SquareState { click }) = click unit
  render ctx = do
    props <- getProps ctx
    textValue <- toString props
    pure $ D.button 
            [ P.className "square" 
            , P._id (squareid props)
            , P.onClick (\_ -> 
                getProps ctx >>= setValue)
            ] 
            [ D.text textValue ]
    

board :: ReactClass BoardState
board = createClass boardSpec 
  where
  boardSpec = (spec initialBoardState render)
    { displayName = "board"}


  hasWinner (squares) =
    winningLines # mapMaybe isWinner # head
    where
      allOf :: String -> Array (Maybe String) -> Maybe String
      allOf value = foldl sameValue (Just value)  
        where 
            sameValue (Just x) (Just y) | x == value && y == x = Just value
            sameValue m n = Nothing
      isWinner :: Array Int -> Maybe String
      isWinner line =  
         line 
           # map (index squares)   
           # (\y -> allOf "X" y <|> allOf "O" y)
  hasWinner' (BoardState {squares}) = 
    case hasWinner squares of
       Just line -> show line 
       Nothing -> "No winner"
  hasWinner'' (BoardState {squares}) = 
    case hasWinner squares of
       Just line -> true
       Nothing -> false

  status :: BoardState -> String
  status (BoardState {squares, xIsNext}) = 
    
    case hasWinner squares of
       Just line -> "Winner: " <> line 
       Nothing -> "Next player: " <> (if xIsNext then "X" else "O")

  readSquare :: forall t1. Int -> BoardState -> Eff (console:: CONSOLE, state :: ReactState ReadOnly | t1) (Maybe String)
  readSquare i (BoardState {squares}) = do
    pure $ index squares i

  newValue xIsNext = if xIsNext then "X" else "O"
  updateSquare i (BoardState {squares, xIsNext}) = do 
    BoardState {squares : mapWithIndex (\j s -> if j == i then newValue xIsNext else s) squares, xIsNext: not xIsNext}
  rendersquare ctx i =                
    createFactory square 
      (SquareState 
        { valueFunction: \_ -> do
            test <- (readSquare i) =<< readState ctx
            pure $ test
        , click: \_ -> do
            rState <- readState ctx
            writeStateWithCallback ctx (if (not $ hasWinner'' rState) then updateSquare i rState else rState) (pure unit) >>= hasWinner' >>> log
        , test: show i    
        })

  boardRow ctx row =
    D.div [P.className "board-row"] $ map (\i -> rendersquare ctx $ row * boardSize + i ) (0..(boardSize - 1))

  

  render ctx = do
    state' <- readState ctx
    pure $ D.div [] $ 
        D.div [P.className "status"] [D.text (status state')] : map (boardRow ctx) (0..(boardSize - 1))
            

game :: forall props. ReactClass props
game = createClass gameSpec
  where
  gameSpec = (spec initialState render)
    { displayName = "game" }
  toString :: GameState -> String
  toString ( GameState { player } ) = player
  render ctx = do
    pure $ D.div[P.className "game"]
                [D.div [P.className "game-board"] 
                       [createFactory board initialBoardState] ]


main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void (elm' >>= render ui)
  where
  ui :: ReactElement
  ui = D.div' [ createFactory game unit ]

  elm' :: Eff (dom :: DOM | eff) Element
  elm' = do
    win <- window
    doc <- document win
    elm <- getElementById (ElementId "example") (documentToNonElementParentNode (htmlDocumentToDocument doc))
    pure $ unsafePartial (fromJust elm)
