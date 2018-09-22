module Lib
    ( floatingMain
    ) where

import Floating.Evaluate
import Floating.Tape
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import System.IO
import System.Environment

floatingMain :: IO ()
floatingMain = do
  (codefile : _) <- getArgs
  withFile codefile ReadMode $ \handle -> do
    input <- hGetContents handle
    execStateT (execFloating input) (initialTape, IM.empty)
    return ()
