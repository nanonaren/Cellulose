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
import Text.PrettyPrint.ANSI.Leijen

type Loc = (Int,Int)
type St s = State (GOL s)
type Board s = M.Map Loc s

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
    numRows :: Int
  , numCols :: Int
  , board :: Board s
  , vfunc :: ([s] -> s)
  , neighbours :: (Loc -> [Loc])
  }

sample = GOL
  {
    numRows = 50
  , numCols = 50
  , board = M.fromList [if x == 5 then ((x,y),1) else ((x,y),0) | x<-[0..49], y<-[0..49]]
  , vfunc = sum
  , neighbours = \l -> [(1,1), l]
  }

main = do
  options <- cmdArgs options
  let st = (run 2)
  save (board st) "temp"
  print (display (board st) 50 50)

--run :: [(Loc, Int)]
run n = execState (sequence_ (replicate n compute) >> lookup (1,1)) sample

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

save :: Show s => Board s -> FilePath -> IO ()
save board filename = writeFile filename (show.M.toList $ board)

display :: Board Int -> Int -> Int -> Doc
display board nrows ncols = vsep $ map (hsep.map format) tbl
    where tbl = chunk ncols.map snd.M.toList $ board
          format 0 = text "."
          format _ = green $ text "O"

chunk _ [] = []
chunk n xs = let (lhs,rhs) = splitAt n xs
             in lhs : chunk n rhs
