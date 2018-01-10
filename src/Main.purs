module Main where

import Prelude
import Game (game)
import React.DOM as D
import Control.Monad.Eff (Eff)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, createFactory)
import ReactDOM (render)

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
