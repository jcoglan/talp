module Runtime where

import Prelude hiding (and, or, not, fst, snd, subtract, head, tail)
import Lambda


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
