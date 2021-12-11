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

  x = X
  num = Num
  add = Op Add
  mul = Op Mul
  sin = Func Sin
  cos = Func Cos

  pi :: Expr
  pi = Num P.pi

  pi_2 :: Expr
  pi_2 = Num (P.pi / 2)

  pi2 :: Expr
  pi2 = Num (P.pi * 2)

  size :: Expr -> Int
  size (Num _)      = 0
  size X            = 0
  size (Op _ e1 e2) = 1 + size e1 + size e2
  size (Func _ e)   = 1 + size e

  -- Part B

  instance Show Expr where
    show = showExpr

  showExpr :: Expr -> String
  showExpr (Num n)        = show n
  showExpr X              = "x"
  showExpr (Op Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
  showExpr (Op Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
  showExpr (Func Sin e)   = "sin " ++ showFactor e
  showExpr (Func Cos e)   = "cos " ++ showFactor e

  showFactor :: Expr -> String
  showFactor (Op op e1 e2) = "(" ++ showExpr (Op op e1 e2) ++ ")"
  showFactor e             = showExpr e

  -- Part C

  evalOp :: Op -> Double -> Double -> Double
  evalOp Add = (+)
  evalOp Mul = (*)

  evalFunc :: Func -> Double -> Double
  evalFunc Sin = P.sin
  evalFunc Cos = P.cos

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

  readExpr :: String -> Maybe Expr
  readExpr s = case parse expr s' of
    Just(e, "") -> Just e
    _           -> Nothing
    where s' = filter (not . isSpace) s

  ---- Part E
  
  instance Arbitrary Expr where
    arbitrary = sized arbExpr

  prop_ShowReadExpr :: Expr -> Bool
  prop_ShowReadExpr e = (assoc e ==) . assoc <$> fromJust $ readExpr (showExpr e)

  assoc :: Expr -> Expr
  assoc (Op Add (Op Add e1 e2) e3) = assoc (Op Add e1 (Op Add e2 e3))
  assoc (Op Add e1 e2)             = Op Add (assoc e1) (assoc e2)
  assoc (Op Mul e1 e2)             = Op Mul (assoc e1) (assoc e2)
  assoc (Func f e)                 = Func f (assoc e)
  assoc e                          = e

  arbExpr :: Int -> Gen Expr
  arbExpr s = frequency [(1, rUnit), (s, rBin s)]
    where rUnit  = frequency [(2, rNum), (2, rVar), (1, rFunc)]
          rNum   = Num <$> (arbitrary :: Gen Double)
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
  simplifyFunc Sin (Num n) = num $ P.sin n
  simplifyFunc Sin e       = sin $ simplify e
  simplifyFunc Cos (Num n) = num $ P.cos n
  simplifyFunc Cos e       = cos $ simplify e
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
  differentiateFunc Sin e = mul (cos e) (differentiate e)
  differentiateFunc Cos e = mul (num $ -1) (mul (sin e) (differentiate e))

  differentiate :: Expr -> Expr
  differentiate (Num n)       = num 0
  differentiate X             = num 1
  differentiate (Op op e1 e2) = simplify $ differentiateOp op e1 e2
  differentiate (Func f e)    = simplify $ differentiateFunc f e

  -- Debug
  -- TODO: Remove the following lines

  test :: Expr
  test = add (mul x (cos x)) (mul (cos x) x)
