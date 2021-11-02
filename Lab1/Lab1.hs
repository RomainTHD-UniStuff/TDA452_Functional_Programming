{- Lab 1
   Date: 01/11/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

import Test.QuickCheck

--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute


stepsPower :: Integer -> Integer -> Integer
stepsPower n k | k < 0 = error "power: negative argument"
stepsPower n k = k + 1



-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product [n | _ <- list]
             where list = [1..k]

power1_2 :: Integer -> Integer -> Integer
power1_2 n k | k < 0 = error "power: negative argument"
power1_2 n k = product (replicate k' n)
               where k' = fromInteger k

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k    = power (n * n) (div k 2)
           | otherwise = n * power n (k-1)

-- D -------------------------
{-

defined :

n = 0 and k = 0 (solution : 1) because it tests the case of k = 0 which leads
 to a solution of 1, no matter the valuation of n.

These following tests ensure that if n = 0, the solution is 0, except if k = 0.
n = 0 and k = 1 (solution : 0)
n = 0 and k = 2 (solution : 0)
n = 0 and k = 3 (solution : 0)

These following tests ensure that if n = 1, the solution is always 1,
 no matter the valuation of k.
n = 1 and k = 0 (solution : 1)
n = 1 and k = 1 (solution : 1)
n = 1 and k = 2 (solution : 1)
n = 1 and k = 3 (solution : 1)

n = 2 and k = 2 (solution : 4) because it's a standard case of the utilisation
 of the power function.
Same for the following evaluations, which are pretty standard :
n = 2 and k = 0 (solution : 1)
n = 2 and k = 1 (solution : 2)
n = 2 and k = 3 (solution : 8)


These following tests ensure that if n<0,
 then the solution is positive if k is even, otherwise the solution is negative.
n = -2 and k = 0 (solution : 1)
n = -2 and k = 1 (solution : -2)
n = -2 and k = 2 (solution : 4)
n = -2 and k = 3 (solution : -8)

undefined :

n = 1 and k = -1 (-> error "negative argument")

-}

--
prop_powers :: Integer -> Integer -> Bool
prop_powers n k | k < 0 = True
prop_powers n k = and comparisons
 where comparisons = [power n k == power1 n k,
                      power n k == power2 n k]

-- quickCheck on prop_powers does not fail because we excluded the case k<0

prop_powers' = prop_powers

--
powerTest :: Bool
powerTest = and [prop_powers n k | n <- n_cases, k <- k_cases, elem (n,k) cases]
            where cases = [
                           (0,0), (0,1), (0,2), (0,3),
                           (1,0), (1,1), (1,2), (1,3),
                           (2,2), ((-2),0), ((-2),1), ((-2),2), ((-2),3), (1,(-1))
                        ]
                  n_cases = [0, 1, 2, (-2)]
                  k_cases = [0, 1, 2, 3, (-1)]

--
powerTest2 :: Bool
powerTest2 = and [
                and [prop_powers (n_cases !! ni) (k_cases !! ki),
                     power (n_cases !! ni) (k_cases !! ki) == cases !! ni !! ki]
                | ni <- [0..length n_cases - 1], ki <- [0..length k_cases - 1]
            ]
          where cases = [
                            [1, 0, 0, 0],
                            [1, 1, 1, 1],
                            [1, 2, 4, 8],
                            [1, -2, 4, -8]
                        ]
                n_cases = [0, 1, 2, (-2)]
                k_cases = [0, 1, 2, 3]
