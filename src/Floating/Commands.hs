{-# LANGUAGE ViewPatterns #-}
module Floating.Commands where

import Floating.Tape
import qualified Data.IntMap.Strict as IM
import Data.Char
import System.Exit
import System.IO
import Control.Monad.State.Strict

type Instructions = [Int]
type Action = Tape -> IO Tape
type CodeMap = IM.IntMap Instructions
type Execution a = StateT (Tape, CodeMap) IO a
type Command = Execution ()

getCh :: Command
getCh = do
  (Tape ls _ rs, table) <- get
  ch <- liftIO getChar
  put (Tape ls (ord ch) rs, table)

putCh :: Command
putCh = do
  (tape@(Tape _ c _), _) <- get
  liftIO $! putChar $! chr c

isEof :: Command
isEof = do
  (Tape ls _ rs, table) <- get
  eof <- liftIO isEOF
  let eofInt = if eof then 1 else 0
  put (Tape ls eofInt rs, table)

halt :: Command
halt = liftIO exitSuccess

callAction :: Command
callAction = do
  (tape@(Tape _ c _), table) <- get
  case IM.lookup c table of
    Nothing -> return ()
    Just code -> codeToCmd code

tapeFnToCmd :: (Tape -> Tape) -> Command
tapeFnToCmd fn = do
  (tape, table) <- get
  put (fn tape, table)

digitToCmd :: Int -> Command
digitToCmd 0 = getCh
digitToCmd 1 = putCh
digitToCmd 2 = tapeFnToCmd moveLeft
digitToCmd 3 = tapeFnToCmd moveRight
digitToCmd 4 = tapeFnToCmd increment
digitToCmd 5 = tapeFnToCmd decrement
digitToCmd 6 = isEof
digitToCmd 7 = callAction
digitToCmd 8 = halt
digitToCmd 9 = tapeFnToCmd id
digitToCmd _ = error "digitToCmd : not a digit"

codeToCmd :: Instructions -> Command
codeToCmd = mapM_ digitToCmd
