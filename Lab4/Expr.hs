import Prelude
import Test.QuickCheck

-- Part A

data Func = Sin
          | Cos
          deriving (Eq)

data Op = Add
        | Mul
        deriving (Eq)

data Expr = Num Double
          | X
          | Op Op Expr Expr
          | Func Func Expr
          deriving (Eq)

x :: Expr
x = X

pi :: Expr
pi = Num Prelude.pi

pi_2 :: Expr
pi_2 = Num (Prelude.pi / 2)

pi2 :: Expr
pi2 = Num (Prelude.pi * 2)

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Op Add

mul :: Expr -> Expr -> Expr
mul = Op Mul

sin :: Expr -> Expr
sin = Func Sin

cos :: Expr -> Expr
cos = Func Cos

size :: Expr -> Int
size (Num _)      = 1
size X            = 1
size (Op _ e1 e2) = size e1 + size e2
size (Func _ e)   = size e

-- Part B

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Op Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr e              = showFactor e

showFactor :: Expr -> String
showFactor (Op Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showFactor (Num n)        = show n
showFactor (Func Sin e)   = "sin " ++ showExpr e
showFactor (Func Cos e)   = "cos " ++ showExpr e
showFactor X              = "x"
showFactor e              = "(" ++ showExpr e ++ ")"

-- Part C

evalOp :: Op -> Double -> Double -> Double
evalOp Add = (+)
evalOp Mul = (*)

evalFunc :: Func -> Double -> Double
evalFunc Sin = Prelude.sin
evalFunc Cos = Prelude.cos

eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval X y = y
eval (Op op e1 e2) y = evalOp op (eval e1 y) (eval e2 y)
eval (Func f e) y = evalFunc f $ eval e y

-- Part D

readExpr :: String -> Maybe Expr
readExpr = undefined

-- Part E

instance Arbitrary Expr where
    arbitrary = sized arbExpr

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

arbExpr :: Int -> Gen Expr
arbExpr = undefined

-- Part F

simplifyOp :: Op -> Expr -> Expr -> Expr

-- basic numbers, n1 + n2 = n1 + n2
simplifyOp Add (Num e1) (Num e2)            = num (e1 + e2)
-- identity, 0 + e = e
simplifyOp Add (Num 0)  e                   = simplify e
-- pull numbers to the left, e + n = n + e
simplifyOp Add e        (Num n)             = simplify $ add (num n) e
-- add numbers in nested additions, n1 + (n2 + e) = (n1 + n2) + e
simplifyOp Add (Num n1) (Op Add (Num n2) e) = simplify $ add (num (n1 + n2)) e
-- pull numbers out of nested additions, e1 + (n + e2) = n + (e1 + e2)
simplifyOp Add e1       (Op Add (Num n) e2) = simplify $ add (num n) (add e1 e2)
-- pull expressions to the left out of nested additions, (e1 + e2) + e3 = e1 + (e2 + e3)
simplifyOp Add (Op Add e1A e1B) e2          = simplify $ add e1A (add e1B e2)
-- factor out common factors, e1 * e3 + e2 * e3 = (e1 + e2) * e3
simplifyOp Add (Op Mul e1A e1B) (Op Mul e2A e2B) | e1B == e2B = simplify $ mul (add e1A e2A) e1B
-- base case for factorization
                                                 | otherwise  = add (mul e1A e1B) (mul e2A e2B)
-- multiply expressions by 2 if equals
simplifyOp Add e1 e2 | e1 == e2             = simplify $ mul (num 2) e1
-- base case for addition
                     | otherwise            = add e1 e2

-- Similar to the addition, but not enough to merge them...
simplifyOp Mul (Num e1) (Num e2)            = num (e1 * e2)
simplifyOp Mul (Num 0)  e                   = num 0
simplifyOp Mul (Num 1)  e                   = simplify e
simplifyOp Mul e        (Num n)             = simplify $ mul (num n) e
simplifyOp Mul (Num n1) (Op Mul (Num n2) e) = simplify $ mul (num (n1 * n2)) e
simplifyOp Mul e1       (Op Mul (Num n) e2) = simplify $ mul (num n) (mul e1 e2)
simplifyOp Mul (Op Mul e1A e1B) e2          = simplify $ mul e1A (mul e1B e2)
simplifyOp Mul e1 e2                        = mul e1 e2
-- TODO: fix "([x * cos x] + [cos x * x])"

simplifyFunc :: Func -> Expr -> Expr
simplifyFunc Sin (Num n) = num $ Prelude.sin n
simplifyFunc Sin e       = Main.sin $ simplify e
simplifyFunc Cos (Num n) = num $ Prelude.cos n
simplifyFunc Cos e       = Main.cos $ simplify e
-- We could maybe simplify sin and cos by using the fact that sin(x) = cos(pi/2 - x) ?
--  Or that sin(pi/2) = 1 and cos(pi/2) = 0 ?

simplify :: Expr -> Expr
simplify (Num n)       = num n
simplify X             = x
simplify (Op op e1 e2) = simplifyOp op (simplify e1) (simplify e2)
simplify (Func f e)    = simplifyFunc f (simplify e)

-- Part G

differentiateOp :: Op -> Expr -> Expr -> Expr
differentiateOp Add e1 e2 = add (differentiate e1) (differentiate e2)
differentiateOp Mul e1 e2 = add (mul (differentiate e1) e2) (mul e1 (differentiate e2))

differentiateFunc :: Func -> Expr -> Expr
differentiateFunc Sin e = mul (Main.cos e) (differentiate e)
differentiateFunc Cos e = mul (num $ -1) (mul (Main.sin e) (differentiate e))

differentiate :: Expr -> Expr
differentiate (Num n)       = num 0
differentiate X             = num 1
differentiate (Op op e1 e2) = simplify $ differentiateOp op e1 e2
differentiate (Func f e)    = simplify $ differentiateFunc f e

-- Debug
-- TODO: Remove the following lines

test :: Expr
test = add (mul x (Main.cos x)) (mul (Main.cos x) x)
