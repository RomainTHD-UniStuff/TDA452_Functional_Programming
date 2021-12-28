{- Lab 4B - Expr.hs
   Date: 28/12/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

module Expr where

import Prelude hiding (sin, cos, pi)
import qualified Prelude as P (sin, cos, pi)
import Parsing
import Data.Maybe (fromJust)
import Data.Char (isSpace)

import Test.QuickCheck

-- Part A

-- Interface

x :: Expr
num :: Double -> Expr
add, mul :: Expr -> Expr -> Expr
sin, cos :: Expr -> Expr

-- Expr data structure

-- Functions acting on one expression
data Func = Sin
          | Cos
          deriving (Eq, Show)

-- Operators acting on two expressions
data Op = Add
        | Mul
    deriving (Eq, Show)

-- Expression
data Expr = Num Double
          | X
          | Op Op Expr Expr
          | Func Func Expr
          deriving (Eq)

-- Expressions onstructors
x = X
num = Num
add = Op Add
mul = Op Mul
sin = Func Sin
cos = Func Cos

-- Expression size
size :: Expr -> Int
size (Num _)      = 0
size X            = 0
size (Op _ e1 e2) = 1 + size e1 + size e2
size (Func _ e)   = 1 + size e

-- Part B

instance Show Expr where
    show = showExpr

-- Show an expression
showExpr :: Expr -> String
showExpr (Num n)        = show n
showExpr X              = "x"
showExpr (Op Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Op Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Func Sin e)   = "sin " ++ showFactor e
showExpr (Func Cos e)   = "cos " ++ showFactor e

-- Show an expression with parentheses
showFactor :: Expr -> String
showFactor (Op op e1 e2) = "(" ++ showExpr (Op op e1 e2) ++ ")"
showFactor e             = showExpr e

-- Part C

-- Evaluate an operator
evalOp :: Op -> Double -> Double -> Double
evalOp Add = (+)
evalOp Mul = (*)

-- Evaluate a function
evalFunc :: Func -> Double -> Double
evalFunc Sin = P.sin
evalFunc Cos = P.cos

-- Evaluate an expression
eval :: Expr -> Double -> Double
eval (Num n) _       = n
eval X y             = y
eval (Op op e1 e2) y = evalOp op (eval e1 y) (eval e2 y)
eval (Func f e) y    = evalFunc f $ eval e y

-- Part D

expr, term, factor :: Parser Expr

expr   = foldl1 (Op Add) <$> chain term (char '+')
term   = foldl1 (Op Mul) <$> chain factor (char '*')
factor = Num <$> readsP
        <|> char 'x' *> return X
        <|> char '(' *> expr <* char ')'
        <|> Func Sin <$> (satStr "sin" *> factor)
        <|> Func Cos <$> (satStr "cos" *> factor)

satStr :: String -> Parser Char
satStr s = foldr1 (<*) (char <$> s)

-- Transform a string into an expression
readExpr :: String -> Maybe Expr
readExpr s = case parse expr s' of
        Just(e, "") -> Just e
        _           -> Nothing
    where s' = filter (not . isSpace) s

---- Part E

instance Arbitrary Expr where
    arbitrary = sized arbExpr

-- Property to check that an expression is displayed the right way
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = (assoc e ==) . assoc <$> fromJust $ readExpr (showExpr e)

-- Helper function, transforms an expression like `(a + b) + (c + d)` into `a + (b + (c + d))`
assoc :: Expr -> Expr
assoc (Op o1 (Op o2 e1 e2) e3) | o1 == o2 = assoc (Op o1 e1 (Op o2 e2 e3))
                               | otherwise = Op o1 (assoc (Op o2 e1 e2)) (assoc e3)
assoc (Op o e1 e2)             = Op o (assoc e1) (assoc e2)
assoc (Func f e)               = Func f (assoc e)
assoc e                        = e

-- Arbitrary expression
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(2, rUnit), (s, rBin s)]
    where rUnit  = frequency [(2, rNum), (2, rVar), (1, rFunc)]
          -- Helps to generate readable numbers
          rNum   = Num . fromIntegral <$> (arbitrary :: Gen Integer)
          rVar   = return X
          rFunc  = do
              func <- elements [Sin, Cos]
              e <- arbExpr s
              return $ Func func e
          rBin s = do
              op <- elements [Add, Mul]
              let s' = s `div` 2
              e1 <- arbExpr s'
              e2 <- arbExpr s'
              return $ Op op e1 e2

-- Part F

-- Simplify an operator
simplifyOp :: Op -> Expr -> Expr -> Expr
simplifyOp Add e1 e2 = simplifyAdd e1 e2
simplifyOp Mul e1 e2 = simplifyMul e1 e2

simplifyAdd :: Expr -> Expr -> Expr
-- basic numbers, n1 + n2 = n1 + n2
simplifyAdd (Num e1) (Num e2)             = num (e1 + e2)
-- identity, 0 + e = e
simplifyAdd (Num 0)  e                    = e
-- pull numbers to the left, e + n = n + e
simplifyAdd e        (Num n)              = simplifyAdd (num n) e
-- add numbers in nested additions, n1 + (n2 + e) = (n1 + n2) + e
simplifyAdd (Num n1) (Op Add (Num n2) e)  = simplifyAdd (num (n1 + n2)) e
-- pull numbers out of nested additions, e1 + (n + e2) = n + (e1 + e2)
simplifyAdd e1       (Op Add (Num n) e2)  = simplifyAdd (num n) (add e1 e2)
simplifyAdd e1       e2                   = add e1 e2

simplifyMul :: Expr -> Expr -> Expr
-- Similar to the addition, but not enough to merge the two cases...
simplifyMul (Num e1) (Num e2)            = num (e1 * e2)
simplifyMul (Num 0)  e                   = num 0
simplifyMul (Num 1)  e                   = e
simplifyMul e        (Num n)             = simplifyMul (num n) e
simplifyMul (Num n1) (Op Mul (Num n2) e) = simplifyMul (num (n1 * n2)) e
simplifyMul e1       (Op Mul (Num n) e2) = simplifyMul (num n) (mul e1 e2)
simplifyMul e1       e2                  = mul e1 e2

-- Simplify a function
simplifyFunc :: Func -> Expr -> Expr
simplifyFunc Sin (Num n) = num $ P.sin n
simplifyFunc Sin e       = sin e
simplifyFunc Cos (Num n) = num $ P.cos n
simplifyFunc Cos e       = cos e

-- Simplify an expression, assuming a well-formed expression like `a + (b + (c + d))`
basicSimplify :: Expr -> Expr
basicSimplify (Num n)       = num n
basicSimplify X             = x
basicSimplify (Op op e1 e2) = simplifyOp op (basicSimplify e1) (basicSimplify e2)
basicSimplify (Func f e)    = simplifyFunc f (basicSimplify e)

-- Simplify an expression
simplify :: Expr -> Expr
simplify e = assoc $ basicSimplify $ assoc e

-- Property to check that an expression is simplified the right way
--  Because of some floating point errors, we have to check that small numbers
--  like 1e-7 and 1e-8 are absolutely almost equal (i.e. a difference < 1e-6 for example),
--  and that big numbers like 1e7 and 1e8 are relatively almost equal
--  (for example, we can allow a difference of 0.1 if the numbers are in the range of 1e35).
--  This second method shouldn't be used for small numbers, because they have a terrible condition
--  and could skyrocket to infinity for numbers near zero
--  We suspect Haskell's prelude or even GHC itself to do some background optimizations,
--  expecially on sin and cos, to avoid these floating point errors.
prop_simplify :: Expr -> Double -> Bool
prop_simplify e x = if abs e1 < 1.0 then abs (e1 - e2) < eps else abs (abs(e1 / e2) - 1.0) < eps
    where e1 = eval e x
          e2 = eval (simplify e) x
          eps = if abs e1 > 1e18 then 1e-3 else 1e-5

-- Helper function, checks whether an expression is a numeric or not
isNumeric :: Expr -> Bool
isNumeric (Num n) = True
isNumeric e       = False

-- Checks whether an operation contains junks or not
containsJunkOp :: Op -> Expr -> Expr -> Bool
containsJunkOp _   (Num _) (Num _)            = True              -- `n + m` or `n * m`
containsJunkOp _   (Num 0) _                  = True              -- `0 + e` or `0 * e`
containsJunkOp Mul (Num 1) _                  = True              -- `1 * e`
containsJunkOp _   e1      e2                 = containsJunk e1 || containsJunk e2

-- Checks whether an expression contains junks or not
containsJunk :: Expr -> Bool
containsJunk (Op op e1 e2) = containsJunkOp op e1 e2
containsJunk (Func f e)    | isNumeric e = True
                           | otherwise   = containsJunk e
containsJunk (Num _)        = False
containsJunk X              = False

-- Property to check that there is no more remaining junks after simplifying an expression
prop_max_simplify :: Expr -> Bool
prop_max_simplify e = not $ containsJunk $ simplify e

-- Part G

-- Derive an operator
differentiateOp :: Op -> Expr -> Expr -> Expr
differentiateOp Add e1 e2 = add (differentiate e1) (differentiate e2)
differentiateOp Mul e1 e2 = add (mul (differentiate e1) e2) (mul e1 (differentiate e2))

-- Derive a function
differentiateFunc :: Func -> Expr -> Expr
differentiateFunc Sin e = mul (cos e) (differentiate e)
differentiateFunc Cos e = mul (num $ -1) (mul (sin e) (differentiate e))

-- Derive an expression
differentiate :: Expr -> Expr
differentiate (Num n)       = num 0
differentiate X             = num 1
differentiate (Op op e1 e2) = simplify $ differentiateOp op e1 e2
differentiate (Func f e)    = simplify $ differentiateFunc f e