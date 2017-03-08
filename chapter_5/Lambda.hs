module Lambda where

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


-- Syntax sugar

var c         = Var [c]

lam (c:[]) b  = Lambda [c] b
lam (c:cs) b  = Lambda [c] $ lam cs b

f <~ a = Apply f a
infixl 1 <~
