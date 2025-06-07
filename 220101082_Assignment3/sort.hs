{-
1. To run testcases:
   ghc -o sort sort.hs
   ./sort
2. To call the function
   ghci
   :l "sort.hs"
   quickSort [..]
3. Input Constraint: It requires a list of integers.
-}

-- The following code implements the quick sort algorithm
{-
-- Without List Comprehension
-- Here filter condition list : it returns elements which satisfies the condition
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort array = 
    let pivot = last array
        remaining = init array
        left = filter (<=pivot) remaining
        right = filter (>pivot) remaining
    in quickSort (left) ++ [pivot] ++ quickSort (right)
-}

-- With List Comprehension
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort array = 
    let pivot = last array
        remaining = init array
        left = [x | x <- remaining, x <= pivot] 
        right = [x | x <- remaining, x > pivot] 
    in quickSort (left) ++ [pivot] ++ quickSort (right)

-- This prints the test cases
main :: IO ()
main = do
    putStrLn ("Running the test cases: ")
    putStrLn ("[1,5,2,8,4,3] -> After Sorting is " ++ show (quickSort [1,5,2,8,4,3]))
    putStrLn ("[1,5,1001,8,100,3] -> After Sorting is " ++ show (quickSort [1,5,1001,8,100,3]))
    putStrLn ("[1,1,1,1,1] -> After Sorting is " ++ show (quickSort [1,1,1,1,1]))
    putStrLn ("[] -> After Sorting is " ++ show (quickSort []))
    putStrLn ("[1] -> After Sorting is " ++ show (quickSort [1]))
    putStrLn ("[1,5,2,-8,4,3] -> After Sorting is " ++ show (quickSort [1,5,2,-8,4,3]))