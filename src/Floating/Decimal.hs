{-# LANGUAGE StrictData #-}
module Floating.Decimal where

import Data.List
import Data.Ratio
import Control.Monad
import Control.Monad.State.Strict
import Floating.Tape

data Decimal = Decimal
             { intPart :: Integer
             , decimalPart :: [Int]
             }

ratioToDecimal :: Rational -> Decimal
ratioToDecimal r = Decimal
                 { intPart = int
                 , decimalPart = map fromInteger $! unfoldr step rest
                 }
  where num = numerator r
        den = denominator r
        (int, rest) = divMod num den
        step 0 = Nothing
        step n = Just $! divMod (10 * n) den
