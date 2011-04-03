module Main
    (
    ) where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

type Loc = (Int,Int)
type St s = State (GOL s)

data GOL s = GOL
  {
    board :: M.Map Loc s
  , vfunc :: ([s] -> s)
  , neighbours :: (Loc -> [Loc])
  }

sample = GOL
  {
    board = M.fromList [((x,y),0) | x<-[1..10], y<-[1..10]]
  , vfunc = undefined
  , neighbours = undefined
  }

run :: Loc -> Int
run loc = evalState (lookup loc) sample

lookup :: Loc -> St s s
lookup loc = gets (fromJust.M.lookup loc.board)

