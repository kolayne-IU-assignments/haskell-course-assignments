plusInf = 1 / 0  -- :: (Fractional a) => a

applyNewton :: Fractional x => (x -> x) -> (x -> x) -> x -> x
applyNewton f f' x = x - (f x) / (f' x)


-- | Find a root of an equation using Newton's method.
root
    :: Fractional a
    => Integer             -- ^ Maximum number of iterations.
    -> (a -> Bool)         -- ^ An acceptable error (precusion).
    -> (a -> a)            -- ^ Function f to find zero for.
    -> (a -> a)            -- ^ Derivative f' of function f
    -> a                   -- ^ Initial guess
    -> Maybe (Integer, a)  -- ^ Number of iterations and root,
                           -- if found
root iters precisAccept f f' x0
    | iters <= 1          = Nothing
    | precisAccept ansErr = Just (ansIter, ansX)
    | otherwise           = Nothing
    where
        itersTimes = [1..iters]
        (ansX, ansErr, ansIter) = foldl applyNext (x0, 1 / 0, 0) itersTimes
        --applyNext :: Fractional a => (a, a, Integer) -> Integer -> (a, a, Integer)
        applyNext prev@(x, err, _) curIter =
            if precisAccept err then
                prev
            else
                let newX = applyNewton f f' x in (newX, abs (x - newX), curIter)


main :: IO ()
main = do
    print $ root 100 (< 1e-7) (\x -> x^2 - 2) (\x -> 2*x) 123
    print $ root 100 (< 1e-12) cos (negate . sin) 1.0
    print $ root 100 (< 1e-12) cos (negate . sin) 0
