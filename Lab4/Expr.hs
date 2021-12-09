data Expr = Num Double
          | X
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq)

x :: Expr
x = X

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Add

mul :: Expr -> Expr -> Expr
mul = Mul

sin :: Expr -> Expr
sin = Sin

cos :: Expr -> Expr
cos = Cos

size :: Expr -> Int
size (Num _) = 1
size X = 1
size (Add e1 e2) = size e1 + size e2
size (Mul e1 e2) = size e1 + size e2
size (Sin e) = size e
size (Cos e) = size e
