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
import System.Console.ANSI
import System.Process (system)
import Text.PrettyPrint.ANSI.Leijen

type Loc = (Int,Int)
type St s = State (GOL s)
type Board s = M.Map Loc s

data Options = Options
  {
    iterations :: Int
  , steptime :: Double
  } deriving (Data,Typeable)

options = Options
  {
    iterations = def &= help "Number of iterations" &= typ "INT"
  , steptime = def &= help "Time to pause between iterations in seconds"
                   &= typ "DOUBLE"
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

sRows = 10
sCols = 10
sample = GOL
  {
    numRows = sRows
  , numCols = sCols
  , board = M.fromList [if x == 5 then ((x,y),1) else ((x,y),0) | x<-[0..sRows-1], y<-[0..sCols-1]]
  , vfunc = flip mod 2.sum
  , neighbours = \(x,y) -> [(x,y),(x,mod (y+1) sCols),(x,mod (y-1) sCols),
                            (mod (x+1) sRows,y),(mod (x-1) sRows, y)]
  }

main = do
  args <- cmdArgs options
  display 0 (board sample) sRows sCols
  run args

run args = foldM_ f sample [1..iterations args]
    where f s i = do
            let s' = execState compute s
            clear (numRows s') (numCols s')
            display i (board s') (numRows s') (numCols s')
            sleep
            return s'
          sleep = system $ "sleep " ++ show (steptime args)

display i brd nrows ncols = print $ createDoc i brd nrows ncols
clear nrows ncols = do
  cursorUp (nrows+1)
  cursorBackward (ncols*2 -1)

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

createDoc :: Int -> Board Int -> Int -> Int -> Doc
createDoc iter board nrows ncols =
    (header <$>).vsep.map (hsep.map format) $ tbl
    where tbl = chunk ncols.map snd.M.toList $ board
          format 0 = text "."
          format _ = green.text $ "O"
          header = text "Iteration:" <+> int iter

chunk _ [] = []
chunk n xs = let (lhs,rhs) = splitAt n xs
             in lhs : chunk n rhs
