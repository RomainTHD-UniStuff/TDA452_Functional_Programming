import Prelude hiding (sin, cos)

-- Part A

data Function = Sin
              | Cos
              deriving (Eq)

data Operator = Add
              | Mul
              deriving (Eq)

data Expr = Num Double
          | X
          | Operator Operator Expr Expr
          | Function Function Expr
          deriving (Eq)

x :: Expr
x = X

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Operator Add

mul :: Expr -> Expr -> Expr
mul = Operator Mul

sin :: Expr -> Expr
sin = Function Sin

cos :: Expr -> Expr
cos = Function Cos

size :: Expr -> Int
size (Num _) = 1
size X = 1
size (Operator _ e1 e2) = size e1 + size e2
size (Function _ e) = size e

-- Part B

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Operator Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr e                    = showFactor e

showFactor :: Expr -> String
showFactor (Operator Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showFactor (Num n)              = show n
showFactor (Function Sin e)     = "sin " ++ showExpr e
showFactor (Function Cos e)     = "cos " ++ showExpr e
showFactor X                    = "x"
showFactor e                    = "(" ++ showExpr e ++ ")"
