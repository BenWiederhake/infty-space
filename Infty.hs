module Infty
( Infty -- without constructor
, toInfty
, fromInfty
, (.+.)
, negateInfty
, (.-.)
, (.*.)
, recipInfty
, (./.)
) where

data Infty = Infty {real :: Float, inftyness :: Int} deriving (Show)

toInfty :: Float -> Infty
toInfty 0 = Infty 1 (-1) -- invent some information
toInfty x = Infty x 0
-- Can't properly handle 0, NaN, Inf, or -Inf
-- Handle 0 by pretending it's a "single" zero.

fromInfty :: Infty -> Float
fromInfty x = case compare (inftyness x) 0 of
    LT -> 0 -- lose some information.  Yay symmetry!
    EQ -> real x
    GT -> 1/0

(.+.) :: Infty -> Infty -> Infty
x .+. y =
    case compare (inftyness x) (inftyness y) of
        LT -> y
        EQ -> let a_sum = (real x + real y)
              in  if a_sum == 0
                  then Infty 1 (inftyness y - 1)
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
