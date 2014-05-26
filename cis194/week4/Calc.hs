module Calc where

import ExprT
import Parser


eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)


evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of
                Nothing -> Nothing
                Just y  -> Just (eval y)

class Expr expr where
    lit :: Integer -> expr
    add :: expr -> expr -> expr    
    mul :: expr -> expr -> expr    

instance Expr ExprT where
    lit x = Lit x
    add x y= Add x y
    mul x y= Mul x y

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit x = x
    add x y= x + y
    mul x y= x * y

instance Expr Bool where
    lit x = x > 0
    add x y= x || y
    mul x y= x && y

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7  ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7  ((x * y) `mod` 7)




testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


