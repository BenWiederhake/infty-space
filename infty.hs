type Infty = (Float, Int)

(.+.) :: Infty -> Infty -> Infty
(a_0, b_0) .+. (a_1, b_1) =
    case compare b_0 b_1 of
        LT -> (a_1, b_1)
        EQ -> let a_sum = (a_0 + a_1)
              in  if a_sum == 0 then (1,-1) else (a_sum, b_0)
        GT -> (a_0, b_0)

negateInfty :: Infty -> Infty
negateInfty (a, b) = (-a, b)

(.-.) :: Infty -> Infty -> Infty
x .-. y = x .+. negateInfty y

(.*.) :: Infty -> Infty -> Infty
(a_0, b_0) .*. (a_1, b_1) = (a_0 * a_1, b_0 + b_1)

recipInfty :: Infty -> Infty
recipInfty (a, b) = (1/a, -b)

(./.) :: Infty -> Infty -> Infty
x ./. y = x .*. recipInfty y
