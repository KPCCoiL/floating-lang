{-# LANGUAGE ViewPatterns #-}
module Floating.Evaluate where

import Floating.Parser
import Floating.Commands
import Floating.Decimal
import Floating.Tape
import Data.Ratio
import Control.Monad
import Control.Monad.State.Strict
import System.Exit
import qualified Data.IntMap as IM

execFloating :: String -> Execution ()
execFloating (lines -> ls) =
  forM_ (zip [1..] ls) $ \(i, line) ->
    case parseLine i line of
      Left err ->
        liftIO $ do
          print err
          exitFailure
      Right expr -> do
        (Tape _ c _, _) <- get
        execRatio $! evalExpr (fromIntegral c) expr

execRatio :: Rational -> Execution ()
execRatio (ratioToDecimal -> Decimal 0 code) = codeToCmd code
execRatio (ratioToDecimal -> Decimal reg code) = do
  (tape, table) <- get
  put (tape, IM.insert (fromInteger reg) code table)

-- we have to define this because Ratio is not Floating. lol
power :: Rational -> Rational -> Rational
power a b
  | denominator b == 1 = a ^ numerator b
  | otherwise = error "^ : exponent not integral"

evalExpr :: Integer -> ExprAST -> Rational
evalExpr c (Literal i) = i % 1
evalExpr c Current = c % 1
evalExpr c (Negate expr) = - evalExpr c expr
evalExpr c (Power base x) = evalExpr c base `power` evalExpr c x
evalExpr c (Mul x y) = evalExpr c x * evalExpr c y
evalExpr c (Div x y) = evalExpr c x / evalExpr c y
evalExpr c (Add x y) = evalExpr c x + evalExpr c y
evalExpr c (Sub x y) = evalExpr c x - evalExpr c y
