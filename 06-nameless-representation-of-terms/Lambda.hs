module Lambda where

import Data.List (elemIndex)


data LTerm = IVar Int
           | CVar Char
           | Lambda Char LTerm
           | Apply LTerm LTerm
           | DiffTerm LTerm
           deriving (Eq)

instance Show LTerm where
  show t = showT [] t
    where
      showT                       :: [Char] -> LTerm -> String
      showT vars (DiffTerm t)     =  "\x1b[4m" ++ showT vars t ++ "\x1b[24m"
      showT vars (IVar n)         =  [vars !! n]
      showT vars (CVar x)         =  ['[', x, ']']
      showT vars (Lambda x body)  =  ['λ', x] ++ ". " ++ showT (x : vars) body
      showT vars (Apply f arg)    =  left f ++ " " ++ right arg
        where
          left t@(Lambda _ _)     =  parens $ showT vars t
          left t                  =  showT vars t

          right t@(IVar _)        =  showT vars t
          right t@(CVar _)        =  showT vars t
          right t                 =  parens $ showT vars t

          parens s                =  "(" ++ s ++ ")"


betaReduce :: LTerm -> LTerm
betaReduce (Apply (Lambda _ body) arg) = sub 0 body
  where
    sub                       :: Int -> LTerm -> LTerm
    sub n (Apply f arg)       =  Apply (sub n f) (sub n arg)
    sub n (Lambda x body)     =  Lambda x $ sub (n + 1) body
    sub n t@(IVar m)
      | m == n                =  shift n 0 arg
      | m > n                 =  IVar (m - 1)
      | otherwise             =  t
    sub _ t                   =  t

    shift n d (Apply f arg)   =  Apply (shift n d f) (shift n d arg)
    shift n d (Lambda x body) =  Lambda x $ shift n (d + 1) body
    shift n d t@(IVar m)
      | m >= d                =  IVar (m + n)
      | otherwise             =  t
    shift _ _ t               =  t

betaReduce t                  =  t


deBruijn                        :: [Char] -> LTerm -> LTerm
deBruijn bound (Lambda x body)  =  Lambda x $ deBruijn (x : bound) body
deBruijn bound (Apply f arg)    =  Apply (deBruijn bound f) (deBruijn bound arg)
deBruijn bound (CVar x)         =  var (elemIndex x bound)
  where
    var (Just n)                =  IVar n
    var Nothing                 =  CVar x
deBruijn _ t                    =  t

lam (c:[]) b = deBruijn [] $ Lambda c b
lam (c:cs) b = deBruijn [] $ Lambda c $ lam cs b

var c = CVar c

f <~ a = Apply f a
infixl 1 <~
