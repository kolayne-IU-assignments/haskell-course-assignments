{-# LANGUAGE InstanceSigs #-}

-- Group 1

data I a = I a

instance Functor I where
    fmap :: (a -> b) -> (I a) -> (I b)
    fmap f (I a) = I (f a)

data S a = S a (S a)

instance Functor S where
    fmap f (S a s) = S (f a) (fmap f s)

data T a b = T a (T b a) | N

instance Functor (T a) where
    fmap _f N = N
    fmap f (T a nextT) = T a (processNext nextT)
        where
            --processNext :: (T b a) -> (T c a)
            processNext N = N
            processNext (T fixMe nextGoodT) = T (f fixMe) (fmap f nextGoodT)

data C a b = C a

instance Functor (C a) where
    fmap _f (C a) = C a

data P a = P

instance Functor P where
    fmap _f P = P

-- Not parametrized, so no functor exists
data U = U


-- Group 2

data F a b = F (a -> b)

instance Functor (F a) where
    fmap f (F aToB) = F (f . aToB)

-- Must accept the same type as it returns, so no functor exists??
data E a = E (a -> a)

-- Same as E?.. Or is it not?..
data Iso a b = Iso (a -> b) (b -> a)

-- f is (b -> c), but I need (c -> b)...
data G a b = G ((b -> a) -> a)

instance Functor (G a) where
    fmap bToC (G bToAToA) = G (cToAToA)
        where
            cToAToA cToA = bToAToA (cToA . bToC)


-- Group 3

data I' f a = I' (f a)

instance Functor f => Functor (I' f) where
    fmap :: (a -> b) -> I' f a -> I' f b
    fmap mapper (I' fa) = I' (fmap mapper fa)

data J f g a = J (f (g a))

instance (Functor f, Functor g) => Functor (J f g) where
    fmap :: (a -> b) -> J f g a -> J f g b
    fmap mapper (J fga) = J (fmap gMapper fga)
        where
            -- gMapper :: g a -> g b
            gMapper = fmap mapper

data K a f b = K a

instance Functor (K a f) where
    fmap _f (K a) = K a

data L a = L a (L a)

instance Functor L where
    fmap f (L a l) = L (f a) (fmap f l)

data M f a = MP a | MF (f (M f a))

instance Functor f => Functor (M f) where
    fmap mapper (MP a) = MP (mapper a)
    fmap mapper (MF fM) = MF (fmap (fmap mapper) fM)


main = return ()
