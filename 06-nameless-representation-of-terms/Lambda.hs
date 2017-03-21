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
betaReduce (Apply (Lambda _ body) s) = sub 0 body
  where
    sub                       :: Int -> LTerm -> LTerm
    sub j (Apply f arg)       =  Apply (sub j f) (sub j arg)
    sub j (Lambda x body)     =  Lambda x $ sub (j + 1) body
    sub j t@(IVar k)
      | k == j                =  shift j 0 s
      | k > j                 =  IVar (k - 1)
      | otherwise             =  t
    sub _ t                   =  t

    shift d c (Apply f arg)   =  Apply (shift d c f) (shift d c arg)
    shift d c (Lambda x body) =  Lambda x $ shift d (c + 1) body
    shift d c t@(IVar k)
      | k >= c                =  IVar (k + d)
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
