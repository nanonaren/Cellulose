{-# LANGUAGE DeriveDataTypeable, TupleSections #-}
module Main
    (
      main
    ) where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import System.Console.CmdArgs

type Loc = (Int,Int)
type St s = State (GOL s)

data Options = Options
  {
  } deriving (Data,Typeable)

options = Options
  {
  } &= program "gol"
    &= summary "Game of Life"
    &= details ["http://github.com/nanonaren/Cellulose"]

data GOL s = GOL
  {
    board :: M.Map Loc s
  , vfunc :: ([s] -> s)
  , neighbours :: (Loc -> [Loc])
  }

sample = GOL
  {
    board = M.fromList [((x,y),1) | x<-[1..10], y<-[1..10]]
  , vfunc = sum
  , neighbours = \l -> [(1,1), l]
  }

main = do
  options <- cmdArgs options
  print (run 2)

--run :: [(Loc, Int)]
run n = evalState (sequence_ (replicate n compute) >> lookup (1,1)) sample

lookup :: Loc -> St s s
lookup loc = gets (fromJust.M.lookup loc.board)

getNeighbours :: Loc -> St s [Loc]
getNeighbours loc = gets (flip neighbours loc)

newCellVal :: Loc -> St s s
newCellVal loc = do
    vals <- getNeighbours loc >>= mapM lookup
    gets (flip vfunc vals)

compute :: St s ()
compute = do
    ks <- gets (M.keys.board)
    vals <- mapM (\k -> liftM (k,) $ newCellVal k) ks
    modify (\st -> st{board=M.fromList vals})

--newcellval = getN loc >>= \ns -> mapM lookup ns >>= \vals -> get >>= \st -> 
