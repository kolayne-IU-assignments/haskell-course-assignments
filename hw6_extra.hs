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
evalWith d vars (Add e1 e2) = evalWith d vars e1 + evalWith d vars e2
evalWith d vars (Mul e1 e2) = evalWith d vars e1 * evalWith d vars e2
evalWith default_ vars (Var var) =
    case lookup var vars of
         Nothing -> default_
         Just x -> x

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

defaultLookup :: Eq a => b -> [(a, b)] -> a -> b
defaultLookup default_ keyValues key =
    case lookup key keyValues of
         Nothing -> default_
         Just x -> x

-- | Replace variables with expressions of other variables
expandVars :: Eq a => Expr b -> [(a, Expr b)] -> Expr a -> Expr b
expandVars default_ repls expr = expand (fmap (defaultLookup default_ repls) expr)
    where
        expand :: Expr (Expr a) -> Expr a
        expand (Lit n) = Lit n
        expand (Add a b) = Add (expand a) (expand b)
        expand (Mul a b) = Mul (expand a) (expand b)
        expand (Var e) = e
{-
expandVars _ _ (Lit n) = Lit n
expandVars d repls (Add e1 e2) = Add (expandVars d repls e1) (expandVars d repls e2)
expandVars d repls (Mul e1 e2) = Mul (expandVars d repls e1) (expandVars d repls e2)
expandVars default_ repls (Var v) =
    case lookup v repls of
         Nothing -> default_
         Just e -> e
-}

data GExpr f a
    = GVar a
    | GOp (f (GExpr f a))

data IExpr expr
    = ILit Integer
    | IAdd expr expr
    | IMul expr expr
    deriving (Show, Functor)

-- | Convert from simple to generalised expression/
fromExpr :: Expr a -> GExpr IExpr a
fromExpr (Lit n) = GOp (ILit n)
fromExpr (Var var) = GVar var
fromExpr (Add e1 e2) = GOp (IAdd (fromExpr e1) (fromExpr e2))
fromExpr (Mul e1 e2) = GOp (IMul (fromExpr e1) (fromExpr e2))

-- | Convert from generalised to simple expression.
toExpr :: GExpr IExpr a -> Expr a
toExpr (GVar var) = Var var
toExpr (GOp (ILit n)) = Lit n
toExpr (GOp (IAdd e1 e2)) = Add (toExpr e1) (toExpr e2)
toExpr (GOp (IMul e1 e2)) = Mul (toExpr e1) (toExpr e2)

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

    let unknown = Var "<unknown>"
    let vars = [("x", y + z), ("y", x + 3)]
    print $ display $ expandVars unknown vars (x * y)
    print $ display $ expandVars unknown vars ((y + z) * (x + 3))

    let uninitialized = Var 0
    let intVars = [("x", 3), ("y", 4)]
    print $ eval (expandVars uninitialized intVars ((y + z) * (x + 3)))
