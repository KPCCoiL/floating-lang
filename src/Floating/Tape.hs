{-# LANGUAGE StrictData #-}
module Floating.Tape where

data Tape = Tape [Int] Int [Int]
          deriving Show

initialTape :: Tape
initialTape = Tape (repeat 0) 0 (repeat 0)

currentCenter :: Tape -> Int
currentCenter (Tape _ n _) = n

moveLeft :: Tape -> Tape
moveLeft (Tape (l : ls) c rs) = Tape ls l $! c : rs

moveRight :: Tape -> Tape
moveRight (Tape ls c (r : rs)) = Tape (c : ls) r rs

increment :: Tape -> Tape
increment (Tape ls c rs) = Tape ls (c + 1) rs

decrement :: Tape -> Tape
decrement (Tape ls c rs) = Tape ls (c - 1) rs
