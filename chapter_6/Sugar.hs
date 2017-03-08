module Sugar where

import Prelude hiding (and, or, not, subtract, fst, snd, head, tail)
import Lambda


-- Boolean logic

tru = lam "tf" $ var 't'
fls = lam "tf" $ var 'f'

test = lam "lmn" $ var 'l' <~ var 'm' <~ var 'n'

and = lam "bc" $ var 'b' <~ var 'c' <~ fls
or  = lam "bc" $ var 'b' <~ tru <~ var 'c'
not = lam "a" $ var 'a' <~ fls <~ tru


-- Pairs

pair = lam "fsb" $ var 'b' <~ var 'f' <~ var 's'
fst  = lam "p" $ var 'p' <~ tru
snd  = lam "p" $ var 'p' <~ fls


-- Numerals
-- note: c0 == fls, c1 == ($)

c           :: Int -> LTerm
c n         =  lam "sz" $ body n
  where
    body 0  =  var 'z'
    body n  =  var 's' <~ body (n - 1)

scc = lam "nsz" $ var 's' <~ (var 'n' <~ var 's' <~ var 'z')

scc' = lam "nsz" $ var 'n' <~ var 's' <~ (var 's' <~ var 'z')

plus  = lam "mnsz" $ var 'm' <~ var 's' <~ (var 'n' <~ var 's' <~ var 'z')
times = lam "mn" $ var 'm' <~ (plus <~ var 'n') <~ c 0

times' = lam "mn" $ var 'm' <~ (var 'n' <~ scc) <~ c 0

power = lam "mn" $ var 'n' <~ (times <~ var 'm') <~ c 1

iszro = lam "m" $ var 'm' <~ (lam "x" $ fls) <~ tru

prd = lam "m" $ fst <~ (var 'm' <~ ss <~ zz)
  where
    zz = pair <~ c 0 <~ c 0
    ss = lam "p" $ pair <~ (snd <~ var 'p') 
                        <~ (scc <~ (snd <~ var 'p'))

subtract = lam "mn" $ var 'n' <~ prd <~ var 'm'

equal = lam "mn" $ and <~ (iszro <~ (subtract <~ var 'm' <~ var 'n'))
                       <~ (iszro <~ (subtract <~ var 'n' <~ var 'm'))

nil   = lam "cn" $ var 'n' -- like zero, fls
cons  = lam "htcn" $ var 'c' <~ var 'h' <~ (var 't' <~ var 'c' <~ var 'n')
isnil = lam "t" $ var 't' <~ (lam "xy" $ fls) <~ tru

head = lam "t" $ var 't' <~ tru <~ tru

tail = lam "t" $ fst <~ (var 't' <~ cc <~ nn)
  where
    nn = pair <~ nil <~ nil
    cc = lam "xp" $ pair <~ (snd <~ var 'p')
                         <~ (cons <~ var 'x' <~ (snd <~ var 'p'))


-- Recursion

fix = lam "f" $ h <~ h
  where
    h = lam "x" $ var 'f' <~ (lam "y" $ var 'x' <~ var 'x' <~ var 'y')
