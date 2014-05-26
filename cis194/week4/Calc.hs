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