module Mini1 (
    gridMap,
    gridMapIf,
    evalExpr,
    getVars,
    evalDeriv,
    parse -- reexported to allow use
    ) where

import Expression
import Parser
import Data.List
import Data.Maybe
-- Do not modify the module declaration and imports above!
-- Also do not change the function signatures and do not
-- remove the dummy implementations of the functions if
-- you want your code to compile.

-- Feel free to import anything else here

-- gridMap (20 points), map function over grid elements
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap islem ll = map (map (islem)) (ll) 

gridHelper :: (a -> Bool) -> (a -> a) -> [a]-> [a]
gridHelper cond islem [] = []
gridHelper cond islem (x:xs) = if (cond x) then
                                        (islem x):(gridHelper cond islem (xs))
                                    else
                                        (x):(gridHelper cond islem (xs))
-- gridMapIf (20 points), map functions over grid elements 
-- that satisfy the predicate provided as the first arg.
gridMapIf :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
gridMapIf cond islem [] = []
gridMapIf cond islem (x:xs) = (gridHelper cond islem x):(gridMapIf cond islem xs)
-- evalExpr (20 points), evaluate the expression by
-- substituting (var, value) pairs given in the first arg.
evalExpr :: [(String, Int)] -> ExprV -> Int
evalExpr ll (Leaf(Constant a))= a
evalExpr ll (Leaf(Variable x))= (fromJust(lookup x ll))
evalExpr ll ( BinaryOperation op (Leaf (Constant a))(Leaf (Constant b))) = if op==Plus then
                                                                                        (a+b)
                                                                                    else
                                                                                        (a*b)
evalExpr ll (UnaryOperation op (Leaf (Constant a))) = (-a)
evalExpr ll (UnaryOperation op (Leaf (Variable a))) = (-(evalExpr ll (Leaf(Variable a))))
evalExpr ll (UnaryOperation op (exp))= (-(evalExpr ll (exp)))
evalExpr ll (BinaryOperation op (ex) (ex1)) = if op ==Plus then
                                                    ((evalExpr ll (ex))+(evalExpr ll (ex1)))
                                                else
                                                    ((evalExpr ll (ex))*(evalExpr ll (ex1)))

getv :: ExprV -> [String]->[String]
getv (Leaf(Variable a)) ll = (ll++[a])
getv (Leaf (Constant a)) ll = ll
getv ( BinaryOperation op (ex1)(ex2)) ll = (getv ex1 ll)++(getv ex2 ll)
getv (UnaryOperation op (ex)) ll = (getv ex ll)

-- getVars (20 points), return the variables contained
-- in the expression in a list (ordered, no duplicates)
getVars :: ExprV -> [String]
getVars aa = (sort(nub(getv aa [])))

-- evalDeriv (20 points), evaluate the first derivative
-- with respect to the variable given in the second
-- arg. using (var, value) pairs given in the first arg.                                                                                
evalDeriv :: [(String, Int)] -> String -> ExprV -> Int
evalDeriv [] var exp = 0
evalDeriv ll var (Leaf(Constant a))=0
evalDeriv ll var (Leaf(Variable c))= if c==var then
                                                1
                                            else
                                                0
evalDeriv ll var (UnaryOperation op exp) =(-(evalDeriv ll var exp))
evalDeriv ll var (BinaryOperation Plus (exp1)(exp2))= ((evalDeriv ll var exp1)+(evalDeriv ll var exp2))
evalDeriv ll var (BinaryOperation Times (ex2) (ex1) )= ((evalExpr ll ex2)*(evalDeriv ll var ex1)+(evalExpr ll ex1)*(evalDeriv ll var ex2))
