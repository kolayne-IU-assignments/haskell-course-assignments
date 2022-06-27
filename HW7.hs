{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}


-- ============ NOT EXTRA =============


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


-- ============ EXTRA =============

-- | A half-closed interval '[from; to)' for values of type `t`.
--
-- Invariant: `from <= to`. If `from == to`, the interval is
-- considered empty.
data Interval t = Interval
                { from :: t  -- ^ Start of an interval.
                , to   :: t  -- ^ End of an interval.
                }

instance Show t => Show (Interval t) where
    show i = "[" ++ show (from i) ++ "; " ++ show (to i) ++ ")"

-- | A set of non-overlapping intervals.
--
-- The invariants for this representation are:
--
-- * the intervals are sorted according to from;
-- * the intervals do not overlap (even at a single point).
newtype IntervalSet t = IntervalSet [Interval t]
    deriving Show

-- | Convert an interval set into a list of pairs representing each interval.
fromIntervalSet :: IntervalSet t -> [(t, t)]
fromIntervalSet (IntervalSet intervals) = map (\i -> (from i, to i)) intervals

-- | Unsafely construct an interval set from a list of pairs.
-- Input list must be ordered by the first component; intervals represented
-- by '[from; to)' must not overlap at any points.
unsafeIntervalSet :: [(t, t)] -> IntervalSet t
unsafeIntervalSet almostIntervals = IntervalSet (map (\(f, t) -> Interval f t) almostIntervals)

-- | Removes empty intervals and merges coinciding intervals to create a more compact
-- | representation of the same set of intervals.
beautify :: Eq t => IntervalSet t -> IntervalSet t
beautify (IntervalSet intervals) = IntervalSet (beautifyL (filter isNotEmpty intervals))
    where
        isNotEmpty (Interval f t) = f /= t
        beautifyL (i1:i2:rest) =
            if to i1 == from i2 then
                beautifyL ((Interval (from i1) (to i2)) : rest)
            else
                i1 : beautifyL (i2:rest)
        beautifyL is = is  -- One or zero intervals

-- | An invariant-preserving union of two interval sets.
union :: Ord t => IntervalSet t -> IntervalSet t -> IntervalSet t
union (IntervalSet s1) (IntervalSet s2) = beautify $ IntervalSet (unionL s1 s2)
    where
        unionL [] is = is
        unionL is [] = is
        unionL ((Interval f1 t1):i1s) ((Interval f2 t2):i2s) = includeNow : unionL newi1s newi2s
            where
                leftMostF  = min f1 f2
                leftMostT  = min t1 t2
                nextF      = max (max f1 f2) leftMostT
                rightMostT = max t1 t2

                includeNow   = Interval leftMostF leftMostT
                includeLater = Interval nextF rightMostT

                (newi1s, newi2s) =
                    if rightMostT == t1 then
                        (includeLater:i1s, i2s)
                    else
                        (i1s, includeLater:i2s)

instance Ord t => Semigroup (IntervalSet t) where
    (<>) = union

instance Ord t => Monoid (IntervalSet t) where
    mempty = IntervalSet []

intervalSet :: Ord t => [(t, t)] -> IntervalSet t
intervalSet = mconcat . map (makeIntervalSet . makeValid)
    where
        -- IMHO, it's a pretty strange feature. I would consider intervals with `from <= to`
        -- property violated as empty and filter them out.
        makeValid (f, t) = (min f t, max f t)

        makeIntervalSet (from, to) = IntervalSet [Interval from to]

-- ============ MAIN =============

i1 = unsafeIntervalSet [(1, 4), (6, 9)]
i2 = unsafeIntervalSet [(2, 5), (7, 8)]

main :: IO ()
main = do
    print $ fromIntervalSet (i1 `union` i2)
    print $ fromIntervalSet (intervalSet [('a', 'k'), ('A', 'Z'), ('z', 'j')])
