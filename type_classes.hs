import Prelude

class Eq' a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    {-# MINIMAL (==) | (/=) #-}

    x == y = not (x Main./= y)
    x /= y = not (x Main.== y)

instance Eq a => Eq' [a] where
    -- (==) :: [a] -> [a] -> Bool
    (==) [] [] = True
    (==) [] (_:_) = False
    (==) (_:_) [] = False
    (==) (x:xs) (y:ys) = x Prelude.== y && xs Main.== ys
    (/=) x y = not (x Main.== y)

main = return ()
