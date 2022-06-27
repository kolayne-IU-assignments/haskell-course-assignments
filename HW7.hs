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
mean :: Fractional a => [a] -> Mean a
mean = mconcat . map makeMean

-- | Intermediate results for count, mean and variance.
data Variance a = Variance Int (Mean a) a
                deriving Show

instance Fractional a => Semigroup (Variance a) where
    (<>) :: Variance a -> Variance a -> Variance a
    (Variance 0 _ _ ) <> v2 = v2
    v1 <> (Variance 0 _ _) = v1
    (Variance n1 y1 v1) <> (Variance n2 y2 v2) = Variance (n1+n2) (y1 <> y2) newV
        where
            newV = (v1 * (fromIntegral n1 - 1) + v2 * (fromIntegral n2 - 1)
                    + delta^2 * fromIntegral (n1*n2) / fromIntegral (n1+n2))
                 / fromIntegral (n1 + n2 - 1)
            delta = getMean y2 - getMean y1

instance Fractional a => Monoid (Variance a) where
    mempty = Variance 0 mempty 0

naiveVariance :: Fractional a => [a] -> Variance a
naiveVariance seq = Variance n (mean seq) ((sum $ map adapt seq) / fromIntegral (n-1))
    where
        n = length seq
        curMean = getMean $ mean seq
        adapt x = (x - curMean)^2

-- | Compute count, mean and variance for a list of values.
variance :: Fractional a => [a] -> Variance a
variance [] = mempty
variance [_] = undefined  -- ??? What's the variance of a singleton set?
    -- If exactly three elements are left, we can't take the first pair
    -- because then there is nothing we could do with the last element.
variance guys@(_:_:_:[]) = naiveVariance guys
    -- Otherwise we take the first pair and do the thing with the rest
variance (a:b:rest) = naiveVariance [a, b] <> variance rest


main :: IO ()
main = print $ variance [1.0, 3, 5]
