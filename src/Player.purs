module Player where

import Prelude
import Data.Maybe (Maybe(..))
-- import Data.Array ((:), (..), mapMaybe, head)
import Data.List (foldl)

data Player = X | O

showPlayer :: Player -> String
showPlayer X = "X"
showPlayer O = "O"

flipPlayer :: Player -> Player
flipPlayer X = O
flipPlayer O = X

samePlayer (Just X) (Just X) = Just X
samePlayer (Just O) (Just O) = Just O
samePlayer m n = Nothing

allOfPlayer :: Player -> Array (Maybe Player) -> Maybe Player
allOfPlayer X = foldl samePlayer (Just X) 
allOfPlayer O = foldl samePlayer (Just O) 
        