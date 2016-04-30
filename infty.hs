type Infty = (Float, Int)

(.+.) :: Infty -> Infty -> Infty
(a_0, b_0) .+. (a_1, b_1) =
    case compare b_0 b_1 of
        LT -> (a_1, b_1)
        EQ -> let a_sum = (a_0 + a_1)
              in  if a_sum == 0 then (0,0) else (a_sum, b_0)
        GT -> (a_0, b_0)

negateInfty :: Infty -> Infty
negateInfty (a, b) = (-a, b)

(.-.) :: Infty -> Infty -> Infty
x .-. y = x .+. negateInfty y

(.*.) :: Infty -> Infty -> Infty
(0, 0) .*. (0, 0) = (0, 0)
(0, _) .*. (0, _) = error "Had a (zero, non-zero)" -- Because it's easy
(a, b) .*. (0,0) = (a, b-1)
(0, 0) .*. (a,b) = (a, b-1) -- Ugly, but I don't know any better
(a_0, b_0) .*. (a_1, b_1) = (a_0 * a_1, b_0 + b_1)

recipInfty :: Infty -> Infty
recipInfty (0, 0) = (1, 1)
recipInfty (a, b) = (1/a, -b)

(./.) :: Infty -> Infty -> Infty
x ./. y = x .*. recipInfty y
