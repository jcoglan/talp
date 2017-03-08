module Lambda where

import Prelude hiding (and, or, not, fst, snd, subtract, head, tail)
import Control.Monad (when)
import Data.List (nub, sort)


data LTerm = Var String
           | Lambda String LTerm
           | Apply LTerm LTerm
           | DiffTerm LTerm
           deriving (Eq)

instance Show LTerm where
  show (DiffTerm t)       =  "\x1b[4m" ++ show t ++ "\x1b[24m"
  show (Var name)         =  name
  show (Lambda x body)    =  "λ" ++ x ++ ". " ++ show body
  show (Apply f arg)      =  left f ++ " " ++ right arg
    where
      left t@(Lambda _ _) =  parens $ show t
      left t              =  show t

      right t@(Var _)     =  show t
      right t             =  parens $ show t

      parens s            =  "(" ++ s ++ ")"


-- Lambda calculus operations

collisions                      :: LTerm -> [String]
collisions (Lambda name body)   =  sort $ nub $ scan [] body
  where
    scan                        :: [String] -> LTerm -> [String]
    scan bound (Apply f arg)    =  scan bound f ++ scan bound arg
    scan bound (Lambda x body)
      | x == name               =  []
      | otherwise               =  scan (x : bound) body
    scan bound (Var x)
      | x == name               =  bound
      | otherwise               =  []
collisions _                    =  []


freeVars                  :: LTerm -> [String]
freeVars t                =  sort $ nub $ scan t
  where
    scan                  :: LTerm -> [String]
    scan (Apply f arg)    =  scan f ++ scan arg
    scan (Lambda x body)  =  filter (/= x) $ scan body
    scan (Var x)          =  [x]


alphaConvert :: [String] -> LTerm -> LTerm
alphaConvert free t@(Lambda name body) = Lambda newName $ rewrite body
  where
    newName                   =  rename name
    avoid                     =  free ++ collisions t

    rename                    :: String -> String
    rename x
      | x `elem` avoid        =  rename (nextName x)
      | otherwise             =  x

    nextName                  :: String -> String
    nextName "z"              =  "a"
    nextName (c:[])           =  [succ c]

    rewrite                   :: LTerm -> LTerm
    rewrite (Apply f arg)     =  Apply (rewrite f) (rewrite arg)
    rewrite t@(Lambda x body)
      | x == name             =  t
      | otherwise             =  Lambda x $ rewrite body
    rewrite t@(Var x)
      | x == name             =  Var newName
      | otherwise             =  t

alphaConvert _ t              =  t


betaReduce :: LTerm -> LTerm
betaReduce (Apply (Lambda name body) arg) = rewrite body
  where
    rewrite                 :: LTerm -> LTerm
    rewrite (Apply f arg)   =  Apply (rewrite f) (rewrite arg)
    rewrite t@(Lambda x _)
      | x == name           =  t
      | otherwise           =  let (Lambda y b) = alphaConvert (freeVars arg) t
                               in Lambda y $ rewrite b
    rewrite t@(Var x)
      | x == name           =  arg
      | otherwise           =  t
betaReduce t                =  t


-- Evaluation strategies

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


-- Evaluation display

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

eval :: (LTerm -> IO ()) -> (LTerm -> LTerm) -> LTerm -> IO ()
eval effect strategy term = do
  effect $ termDiff term reduced
  putStrLn ""
  when (term /= reduced) $ eval effect strategy reduced
  where
    reduced = strategy term

byValue = eval print callByValue
normal  = eval print normalOrder

norm term = do
  if term == reduced then
    print term
  else
    norm reduced
  where
    reduced = normalOrder term


-- Syntax sugar

var c         = Var [c]

lam (c:[]) b  = Lambda [c] b
lam (c:cs) b  = Lambda [c] $ lam cs b

f <~ a = Apply f a
infixl 1 <~


-- Boolean logic

tru = lam "tf" $ var 't'
fls = lam "tf" $ var 'f'

-- test == ($) ($) i.e. ($) for binary functions
test = lam "lmn" $ var 'l' <~ var 'm' <~ var 'n'

and = lam "bc" $ var 'b' <~ var 'c' <~ fls

-- Exercise 5.2.1
or  = lam "bc" $ var 'b' <~ tru <~ var 'c'
not = lam "a" $ var 'a' <~ fls <~ tru


-- Pairs

pair = lam "fsb" $ var 'b' <~ var 'f' <~ var 's'
fst  = lam "p" $ var 'p' <~ tru
snd  = lam "p" $ var 'p' <~ fls


-- Numerals
-- note: c0 == fls, c1 == ($)

-- Exercise 5.2.10
c           :: Int -> LTerm
c n         =  lam "sz" $ body n
  where
    body 0  =  var 'z'
    body n  =  var 's' <~ body (n - 1)

scc = lam "nsz" $ var 's' <~ (var 'n' <~ var 's' <~ var 'z')

-- Exercise 5.2.2
scc' = lam "nsz" $ var 'n' <~ var 's' <~ (var 's' <~ var 'z')

plus  = lam "mnsz" $ var 'm' <~ var 's' <~ (var 'n' <~ var 's' <~ var 'z')
times = lam "mn" $ var 'm' <~ (plus <~ var 'n') <~ c 0

-- Exercise 5.2.3
times' = lam "mn" $ var 'm' <~ (var 'n' <~ scc) <~ c 0

-- Exercise 5.2.4
power = lam "mn" $ var 'n' <~ (times <~ var 'm') <~ c 1

iszro = lam "m" $ var 'm' <~ (lam "x" $ fls) <~ tru

prd = lam "m" $ fst <~ (var 'm' <~ ss <~ zz)
  where
    zz = pair <~ c 0 <~ c 0
    ss = lam "p" $ pair <~ (snd <~ var 'p') 
                        <~ (scc <~ (snd <~ var 'p'))

-- Exercise 5.2.5
subtract = lam "mn" $ var 'n' <~ prd <~ var 'm'

-- Exercise 5.2.7

-- this is infinitely recursive; we need the Y combinator!
equal' = lam "mn" $ or <~ (and <~ (iszro <~ var 'm') <~ (iszro <~ var 'n'))
                       <~ (and <~ (and <~ (nzero <~ var 'm') <~ (nzero <~ var 'n'))
                               <~ (equal' <~ (prd <~ var 'm') <~ (prd <~ var 'n')))
  where
    nzero = lam "m" $ not <~ (iszro <~ var 'm')

-- fails if n > m
equal'' = lam "mn" $ iszro <~ (subtract <~ var 'm' <~ var 'n')

equal = lam "mn" $ and <~ (iszro <~ (subtract <~ var 'm' <~ var 'n'))
                       <~ (iszro <~ (subtract <~ var 'n' <~ var 'm'))

-- Exercise 5.2.8

nil  = lam "cn" $ var 'n' -- like zero, fls
cons = lam "htcn" $ var 'c' <~ var 'h' <~ (var 't' <~ var 'c' <~ var 'n')

-- This is like iszro except the "always return fls" function needs two args,
-- since the 'c' operator on lists is binary whereas 's' in numbers is unary
isnil = lam "t" $ var 't' <~ (lam "xy" $ fls) <~ tru

-- If 't' is a list like 'λc. λn. c x (c y n)', then to get the head we call
-- it with 'c' equal to a function that takes two arguments and returns the
-- first, i.e. c = tru. That reduces 'λc. λn. c x (c y n)' to 'λn. x'. We can
-- then call this with any value since it will be ignored.
head = lam "t" $ var 't' <~ tru <~ tru

-- The same trick won't work for tail, since passing c = fls will recursively
-- select the second value in all pairs, reducing the whole list to just 'n'.
-- We need a trick like prd on numbers.
tail = lam "t" $ fst <~ (var 't' <~ cc <~ nn)
  where
    nn = pair <~ nil <~ nil
    cc = lam "xp" $ pair <~ (snd <~ var 'p')
                         <~ (cons <~ var 'x' <~ (snd <~ var 'p'))


-- Recursion

fix = lam "f" $ h <~ h
  where
    h = lam "x" $ var 'f' <~ (lam "y" $ var 'x' <~ var 'x' <~ var 'y')

equalR = fix <~ (lam "rmn" $
                    or <~ (and <~ (iszro <~ var 'm') <~ (iszro <~ var 'n'))
                       <~ (and <~ (and <~ (nzero <~ var 'm') <~ (nzero <~ var 'n'))
                               <~ (var 'r' <~ (prd <~ var 'm') <~ (prd <~ var 'n'))))
  where
    nzero = lam "n" $ not <~ (iszro <~ var 'n')

-- Exercise 5.2.9

factorial = fix <~ (lam "fn" $
                      test <~ (iszro <~ var 'n')
                           <~ c 1
                           <~ (times <~ var 'n'
                                     <~ (var 'f' <~ (prd <~ var 'n'))))

-- Exercise 5.2.11

-- a slow version based on explicitly destructuring the list
sumlist' = fix <~ (lam "ft" $
                    test <~ (isnil <~ var 't')
                         <~ c 0
                         <~ (plus <~ (head <~ var 't')
                                  <~ (var 'f' <~ (tail <~ var 't'))))

-- better: a 'list' is just a foldr function: we can call it with plus and 0
--    (λc. λn. c x (c y n)) + 0
--  > + x (+ y 0)
sumlist = lam "t" $ var 't' <~ plus <~ c 0
