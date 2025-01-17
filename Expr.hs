{-# LANGUAGE LambdaCase #-}
module Expr where
import Parsing ( char, parse, readsP, zeroOrMore, (<|>), Parser )
import Data.Maybe ( fromJust )
import Test.QuickCheck
    ( arbitraryBoundedEnum, oneof, sized, Arbitrary(arbitrary), Gen, quickCheck )
import Data.Fixed (mod')

-- A ----------------------------------------------------

data BinOp = Add | Mul deriving (Show, Eq, Enum, Bounded)
data UnOp = Sin | Cos deriving (Show, Eq, Enum, Bounded)

data Expr
 = Num Double
 | Binary BinOp Expr Expr
 | Unary UnOp Expr
 | Var
 deriving(Show, Eq)

x :: Expr
x = Var

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Binary Add

mul :: Expr -> Expr -> Expr
mul = Binary Mul

sin :: Expr -> Expr
sin = Unary Sin

cos :: Expr -> Expr
cos = Unary Cos

size :: Expr -> Int
size = \case
  Num _           -> 0
  Var             -> 0
  Binary _ e1 e2  -> 1 + size e1 + size e2
  Unary _ e       -> 1 + size e

-- B ----------------------------------------------------

showExpr :: Expr -> String
showExpr = \case
    Num n           -> show n
    Var             -> "x"
    Binary op e1 e2 -> wrapBinaryIfNeeded e1 op ++ " " ++ showBinOp op ++ " " ++ wrapBinaryIfNeeded e2 op
    Unary op e      -> showUnOp op ++ wrapIfUnaryNeeded e

showBinOp :: BinOp -> String
showBinOp = \case
    Add -> "+"
    Mul -> "*"

showUnOp :: UnOp -> String
showUnOp = \case
    Sin -> "sin"
    Cos -> "cos"

wrapBinaryIfNeeded :: Expr -> BinOp -> String
wrapBinaryIfNeeded e op = case e of
    Binary Add _ _ -> case op of 
        Mul -> "(" ++ showExpr e ++ ")" --if op case of mul, parentheses else not
        _ -> showExpr e
    _ -> showExpr e

wrapIfUnaryNeeded :: Expr -> String
wrapIfUnaryNeeded e = case e of
    Num _ -> " " ++ showExpr e
    Var   -> " " ++ showExpr e
    _     -> "(" ++ showExpr e ++ ")"

-- C ----------------------------------------------------
eval :: Expr -> Double -> Double
eval = \case
    Num n           -> const n
    Unary op e      -> \x -> evalUnOp op (eval e x) 
    Binary op e1 e2 -> \x -> evalBinOp op (eval e1 x) (eval e2 x)
    Var             -> id

evalBinOp :: BinOp -> (Double -> Double -> Double)
evalBinOp = \case
    Add -> (+)
    Mul -> (*)
    
evalUnOp :: UnOp -> (Double -> Double)
evalUnOp = \case
    Sin -> Prelude.sin
    Cos -> Prelude.cos

-- D ----------------------------------------------------
removeWhiteSpace :: String -> String
removeWhiteSpace = filter (/= ' ')

string :: String -> Parser String
string = mapM char

readExpr :: String -> Maybe Expr
readExpr string = case parse addition (removeWhiteSpace string) of
    Just(e, "") -> Just e
    _          -> Nothing

addition, multiplication, number, factor, parenthesis, unaryOp, variable:: Parser Expr
--addition = do
--    t <- multiplication
--    ts <- zeroOrMore (do char '+'; multiplication)
--    return $ foldl (Binary Add) t ts

addition = do
    t <- multiplication
    ts <- zeroOrMore (do char '+'; multiplication)
    return $ foldl (Binary Add) t ts

multiplication = do
    t <- factor
    ts <- zeroOrMore (do char '*'; multiplication)
    return $ foldl (Binary Mul) t ts

number = Num <$> (readsP :: Parser Double)

parenthesis = do
    char '('
    e <- addition
    char ')'
    return e

unaryOp = do
    op <- string "sin" <|> string "cos"
    let unOp = case op of
            "sin" -> Sin
            "cos" -> Cos
    Unary unOp <$> factor

variable = do
    char 'x'
    return Var

factor = number <|> variable <|> unaryOp <|> parenthesis

-- E ----------------------------------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = arrange e == arrange (fromJust (readExpr (showExpr e)))

arrange :: Expr -> Expr
arrange = \case
    (Num n)                          -> Num n
    Binary Mul (Binary Mul e1 e2) e3 -> arrange (Binary Mul e1 (Binary Mul e2 e3))
    Binary Add (Binary Add e1 e2) e3 -> arrange (Binary Add e1 (Binary Add e2 e3))
    Binary op e1 e2                  -> Binary op (arrange e1) (arrange e2)
    Unary op e                       -> Unary op (arrange e)
    Var                              -> Var

instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [Num <$> arbitrary, return Var]
arbExpr n = oneof
  [
    Num <$> arbitrary,
    return Var,
    Binary <$> arbitrary <*> arbExpr (n `div` 2) <*> arbExpr (n `div` 2),
    Unary <$> arbitrary <*> arbExpr (n - 1)
  ]

instance Arbitrary BinOp where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UnOp where
  arbitrary = arbitraryBoundedEnum

-- F ----------------------------------------------------
simplify :: Expr -> Expr
simplify expr =
  let simplified = simplifyOnce expr
  in if simplifyOnce expr == expr
     then expr
     else simplify simplified

simplifyOnce :: Expr -> Expr
simplifyOnce = \case
    Num n                   -> Num n
    Binary Add (Num 0) e         -> simplifyOnce e
    Binary Add e (Num 0)         -> simplifyOnce e
    Binary Mul (Num 0) _         -> Num 0
    Binary Mul _ (Num 0)         -> Num 0
    Binary Mul (Num 1) e         -> simplifyOnce e
    Binary Mul e (Num 1)         -> simplifyOnce e
    Binary Add (Num n1) (Num n2) -> Num (n1 + n2)
    Binary Mul (Num n1) (Num n2) -> Num (n1 * n2)
    Unary Sin (Num n)            -> Num (Prelude.sin (mod' n (2 * pi)))
    Unary Cos (Num n)            -> Num (Prelude.cos (mod' n (2 * pi)))
    Binary op e1 e2              -> Binary op (simplifyOnce e1) (simplifyOnce e2)
    Unary op e                   -> Unary op (simplifyOnce e)
    Var                          -> Var                 

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < epsilon
  where epsilon = 1e-9

prop_simplify_eq :: Expr -> Bool
prop_simplify_eq e = approxEqual (eval e x) (eval (simplify e) x)
 where x = 1

prop_simplify :: Expr -> Bool
prop_simplify e = case simplify e of
    (Binary Add (Num 0) _)       -> False
    (Binary Add _ (Num 0))       -> False
    (Binary Mul (Num 0) _)       -> False
    (Binary Mul _ (Num 0))       -> False
    (Binary Add (Num _) (Num _)) -> False
    (Binary Mul (Num _) (Num _)) -> False
    (Unary Sin (Num n))          -> n > (2 * pi)
    _                            -> True 


-- G ----------------------------------------------------
differentiate :: Expr -> Expr
differentiate = \case
    Num _                -> Num 0
    Var                  -> Num 1
    Binary Add e1 e2     -> simplify $ Binary Add (differentiate e1) (differentiate e2)
    Binary Mul e1 e2     -> simplify $ Binary Add (Binary Mul (differentiate e1) e2) (Binary Mul e1 (differentiate e2))
    Unary Sin e          -> simplify $ Binary Mul (Unary Cos e) (differentiate e)
    Unary Cos e          -> simplify $ Binary Mul (Binary Mul (Num (-1)) (differentiate e)) (Unary Sin e) 

-- differentiate :: Expr -> Expr
-- differentiate = \case
--     (Num n)     -> Num 0
--     (Add e1 e2) -> simplify $ Add (differentiate e1) (differentiate e2)
--     (Mul e1 e2) -> simplify $ Add (Mul (differentiate e1) e2) (Mul e1 (differentiate e2))
--     (Sin e)     -> simplify $ Mul (Cos e) (differentiate e)
--     (Cos e)     -> simplify $ Mul (Mul (Num (-1)) (Sin e)) (differentiate e)
--     Var         -> Num 1