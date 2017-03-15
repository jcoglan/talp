module Eval where

import Lambda

reduceApply                 :: (LTerm -> LTerm) -> LTerm -> LTerm
reduceApply g (Apply f arg)
  | f == reduceF            =  Apply f (g arg)
  | otherwise               =  Apply reduceF arg
  where
    reduceF                 =  g f
reduceApply _ t             =  t

fullBeta                                    :: LTerm -> LTerm
fullBeta redex@(Apply (Lambda _ _) _)       =  betaReduce redex
fullBeta (Apply f arg)                      =  Apply (fullBeta f) (fullBeta arg)
fullBeta (Lambda x body)                    =  Lambda x $ fullBeta body
fullBeta t                                  =  t

normalOrder                                 :: LTerm -> LTerm
normalOrder redex@(Apply (Lambda _ _) _)    =  betaReduce redex
normalOrder t@(Apply _ _)                   =  reduceApply normalOrder t
normalOrder (Lambda x body)                 =  Lambda x $ normalOrder body
normalOrder t                               =  t

callByName                                  :: LTerm -> LTerm
callByName redex@(Apply (Lambda _ _) _)     =  betaReduce redex
callByName t@(Apply _ _)                    =  reduceApply callByName t
callByName t                                =  t

callByValue                                 :: LTerm -> LTerm
callByValue redex@(Apply f@(Lambda _ _) a)
  | a == reduceA                            =  betaReduce redex
  | otherwise                               =  Apply f reduceA
  where
    reduceA                                 =  callByValue a
callByValue t@(Apply _ _)                   =  reduceApply callByValue t
callByValue t                               =  t

termDiff                              :: LTerm -> LTerm -> LTerm
termDiff x@(Apply f a) y@(Apply g b)
  | f /= fdiff && a /= adiff          =  DiffTerm x
  | otherwise                         =  Apply fdiff adiff
  where
    fdiff                             =  termDiff f g
    adiff                             =  termDiff a b
termDiff (Lambda x a) (Lambda y b)    =  Lambda x $ termDiff a b
termDiff x y
  | x == y                            =  x
  | otherwise                         =  DiffTerm x

eval :: (LTerm -> LTerm) -> (LTerm -> IO ()) -> LTerm -> IO ()
eval strategy effect term =
  if term == reduced then
    print term
  else do
    effect (termDiff term reduced)
    eval strategy effect reduced
  where
    reduced = strategy term

noop :: (Monad m) => a -> m ()
noop x = return ()
