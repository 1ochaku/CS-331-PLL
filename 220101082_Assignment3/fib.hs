{-
1. To run testcases:
   ghc -o fib fib.hs
   ./fib
2. To call the function
   ghci
   :l "fib.hs"
   fib (x)
3. Input Constraint: It takes non zero positive number.
-}

-- Fibonacci Sequence considered: 1 1 2 3 ....
{-
    Here the fibSeries is created as per the nth fibonacci number req.
    This is due to the lazy evaluation technique used by Haskel.
    zipWith takes function and two lists.
    Also zipWith stops at the shortest list size.
-}
fib :: Int -> Integer
fib n = fibSeries !! (n-1)
    where
        fibSeries :: [Integer]
        fibSeries = 1 : 1 : zipWith (+) fibSeries (tail fibSeries)

-- This prints the test cases
main :: IO ()
main = do 
    putStrLn ("Running the test cases: ")
    putStrLn ("5th Fibonacci is: " ++ show(fib 5))
    putStrLn ("4th Fibonacci is: " ++ show(fib 4))
    putStrLn ("100th Fibonacci is: " ++ show(fib 100))
    putStrLn ("16th Fibonacci is: " ++ show(fib 16))
    putStrLn ("200th Fibonacci is: " ++ show(fib 200))
    putStrLn ("7th Fibonacci is: " ++ show(fib 7))
    putStrLn ("10th Fibonacci is: " ++ show(fib 10))

{-
To print the fibonacci series:
-- This defines the list
fibSeries :: [Integer]
fibSeries = 1 : 1 : zipWith (+) fibSeries (tail fibSeries)

main :: IO ()
main = print (take n fibSeries)
-}