data BoolExp
    = T
    | F
    | BoolExp :&& BoolExp
    | BoolExp :|| BoolExp
    deriving Show

-- | Perform a single lazy evaluation step for `BoolExp`
step :: BoolExp -> BoolExp
step T = T
step F = F
--
step (F :&& _) = F
step (_ :&& F) = F
step (T :&& T) = T
step (l :&& r) = (step l) :&& (step r)
--
step (T :|| _) = T
step (_ :|| T) = T
step (F :|| F) = F
step (l :|| r) = (step l) :|| (step r)

-- Evaluate a boolean expression step by step until an exact result.
-- Returns all the steps
steps :: BoolExp -> [BoolExp]
steps T = [T]
steps F = [F]
steps expr = expr:(steps (step expr))

-- Pretty-print `steps`
ppSteps :: BoolExp -> IO ()
ppSteps expr = do
        putStr "["
        ppLst (steps expr)
        putStrLn "]"
    where
        ppLst [] = putStr "\b \n"  -- Space overwrites a comma
        ppLst (x:xs) = do
                putStrLn ""
                putStr $ show x
                putStr ","
                ppLst xs

exp1 :: BoolExp
exp1 = T :|| T :|| F :&& (T :|| (F :|| F :&& F) :&& (T :&& T :|| F))

main :: IO ()
main = ppSteps exp1
