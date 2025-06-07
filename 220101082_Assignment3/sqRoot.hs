{-
1. To run testcases:
   ghc -o sqRoot sqRoot.hs
   ./sqRoot
2. To call the function
   ghci
   :l "sqRoot.hs"
   sqroot (x)
3. Input Constraint: It takes only positive decimal numbers.
-}

-- Rounds off the result to 5 decimal places
decimalUpto5 :: Double -> Double
decimalUpto5 a = fromIntegral (round(a*100000)) / 100000

-- Finds the square root of a number using Newton's method
{-
We make a guess of the possible square root in this method.
Then we keep on updating the guess till it satisfies the condition.
-}
sqRoot :: Double -> Either String Double
sqRoot a = 
    if a < 0  
        then Left "Negative numbers not supported."
    else if a == 0 
        then Right 0.0
    else Right (sqrtFind 1.0)
    where
        sqrtFind x =
            if abs (x*x - a) < 0.00001 then decimalUpto5(x)
            else sqrtFind ((x+a/x)/2)

-- This prints the test cases
main :: IO ()
main = do 
    putStrLn ("Running the test cases: ")
    putStrLn ("Square root of 5 is: " ++ show(sqRoot 5))
    putStrLn ("Square root of 4.5 is: " ++ show(sqRoot 4.5))
    putStrLn ("Square root of 100 is: " ++ show(sqRoot 100))
    putStrLn ("Square root of 16 is: " ++ show(sqRoot 16))
    putStrLn ("Square root of 3 is: " ++ show(sqRoot 3))
    putStrLn ("Square root of 7 is: " ++ show(sqRoot 7))
    putStrLn ("Square root of 1000 is: " ++ show(sqRoot 1000))