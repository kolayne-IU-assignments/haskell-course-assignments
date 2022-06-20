{-# LANGUAGE DeriveFunctor #-}

import Data.List (transpose)

-- | A simple expression with variables.
data Expr a
    = Lit Integer            -- ^ Integer literal
    | Var a                  -- ^ Variable
    | Add (Expr a) (Expr a)  -- ^ Addition
    | Mul (Expr a) (Expr a)  -- ^ Multiplication
    deriving (Show, Functor)

instance Num (Expr a) where
    e1 + e2 = Add e1 e2
    e1 * e2 = Mul e1 e2
    fromInteger n = Lit n
    -- TODO: leaving other methods undefined...

-- | Evaluate an expression using an associative list
-- | to lookup variable values and a default value
-- | (in case there is no value in associative list).
evalWith :: Eq var => Integer -> [(var, Integer)] -> Expr var -> Integer
evalWith _ _ (Lit n) = n
evalWith d vs (Add e1 e2) = evalWith d vs e1 + evalWith d vs e2
evalWith d vs (Mul e1 e2) = evalWith d vs e1 * evalWith d vs e2
evalWith default_ vars (Var var) =
    case lookup var vars of
         Nothing -> default_
         (Just x) -> x

-- | Display an expression using a given
-- | display function for variables
displayWith :: (var -> String) -> Expr var -> String
displayWith _ (Lit n) = show n
displayWith shower (Var v) = shower v
displayWith shower (Add a b) = "(" ++ displayWith shower a ++ " + " ++ displayWith shower b ++ ")"
displayWith shower (Mul a b) = displayWith shower a ++ " * " ++ displayWith shower b

-- | Eval in terms of `evalWith`
eval :: Expr Integer -> Integer
eval = evalWith default_ allIntsPaired
    where
        default_ = undefined  -- No default value is ever needed
        allInts = 0 : (concat . transpose) [[1..], [-1, -2 ..]]
        allIntsPaired = zip allInts allInts

-- | Display in terms of `displayWith`
display :: Expr String -> String
display = displayWith id

sampleExpr1 :: Expr String
sampleExpr1 = Add (Lit 1) (Mul (Var "x") (Var "width"))

sampleExpr2 :: Expr Int
sampleExpr2 = Add (Lit 1) (Mul (Var 0) (Var 1))

main :: IO ()
main = do
    putStrLn $ displayWith id sampleExpr1
    putStrLn $ displayWith show sampleExpr2

    let vars = [("x", 2), ("y", 3)]
    let x = Var "x"; y = Var "y"; z = Var "z"
    print $ evalWith 0 vars (x + y)
    print $ evalWith 0 vars ((x + y)^2 + z)
