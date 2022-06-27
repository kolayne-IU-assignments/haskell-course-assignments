{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

data Mean a = Mean { len :: Int, getMean :: a }
            deriving Show

instance Fractional a => Semigroup (Mean a) where
    (<>) :: Mean a -> Mean a -> Mean a
    -- Unless objects with zero length are processed separately, it seems
    -- that no truly neutral element exists:
    -- If the length of `mempty` is not zero, then when doing `someMean <> mempty`,
    -- the resulting value will have a different length value.
    -- If, instead, the length is zero then `mempty <> someMean` would
    -- cause a division by zero...
    (Mean 0 _) <> x2 = x2
    (Mean n1 y1) <> (Mean n2 y2) = Mean (n1+n2) newVal
        where
            newVal = y1 + delta * fromIntegral n2 / fromIntegral (n1+n2)
            delta = y2 - y1

instance Fractional a => Monoid (Mean a) where
    mempty = Mean 0 0

-- | Construct a singleton `Mean` object
makeMean :: a -> Mean a
makeMean = Mean 1

-- | Calculate the mean of a list with the `Mean` monoid
--
-- >>> mmean [2, 7, 9]
-- 6.0
-- >>> mmean [4, 3]
-- 3.5
-- >>> mmean (replicate 100 1e308)
-- 1.0e308
mmean :: Fractional a => [a] -> a
mmean = getMean . mconcat . map makeMean

main :: IO ()
main = print $ mmean [2, 7, 9]
