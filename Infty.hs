module Infty
( Infty -- without constructor
, toInfty
, fromInfty
, .+.
, negateInfty
, .-.
, .*.
, recipInfty
, ./.
) where

data Infty = Infty {real :: Float, inftyness :: Int} deriving (Show)

toInfty :: Num -> Infty
toInfty 0 = Infty 1 (-1)
toInfty x = Infty x 0

fromInfty :: Infty -> Num
fromInfty x = case compare (inftyness x) 0 of
    LT -> 0
    EQ -> real x
    GT -> 1/0

(.+.) :: Infty -> Infty -> Infty
x .+. y =
    case compare (inftyness x) (inftyness y) of
        LT -> y
        EQ -> let a_sum = (real x + real y)
              in  if a_sum == 0
                  then Infty 1 (-1)
                  else Infty a_sum (inftyness y)
        GT -> x

negateInfty :: Infty -> Infty
negateInfty (Infty a b) = Infty (-a) b

(.-.) :: Infty -> Infty -> Infty
x .-. y = x .+. negateInfty y

(.*.) :: Infty -> Infty -> Infty
Infty a_0 b_0 .*. Infty a_1 b_1 = Infty (a_0 * a_1) (b_0 + b_1)

recipInfty :: Infty -> Infty
recipInfty (Infty a b) = Infty (1/a) (-b)

(./.) :: Infty -> Infty -> Infty
x ./. y = x .*. recipInfty y