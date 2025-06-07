import System.Environment (getArgs)

{-
    ***** How to run the Program
    1. To run testcases:
        - ghc -o tree tree.hs
        - ./tree
    2. To call the function
        - ghci
        - :l "tree.hs"
        - testcases : for running the testcases
        - interactive : for giving the input yourself
    3. Input Constraint:
        - It takes positive numbers without sign and negative numbers with sign.
        - Only integers allowed.
        - Non empty list not allowed.
-}

{-
    Part A: Reading the input from user as list of comma separated numbers
    - It first splits the input based on first comma
    - Make it into two parts: one is single token, and the other is remaining list
    - Then checks if the remaining list is empty
    - If yes, it returns the list of tokens formed
    - Else it drops the comma and parse for next set of tokens
-}
parseTheList input = 
    let (part, rest) = break (== ',') input
    in if null rest
        then [part]
        else part : parseTheList (drop 1 rest)

--      Part B: Creation of BST   
{-
    Defining the BST node data structure
    deriving Show helps us to print the tree
    here node indicates bst can hold any type of values but us constraint to int
    Node is a constructor (note)
-}
data BST node = Empty | Node (BST node) node (BST node) deriving Show

{- 
    Declares the insert function
    Given an element and a tree, it inserts either in right or left of the tree
    Here Ord node means it should belong to Ord type class supporting ordering
    It includes int, double, char, string
-}
insert :: Ord node => node -> BST node -> BST node
insert x Empty = Node Empty x Empty
insert x (Node left y right)
    | x < y = Node (insert x left) y right
    | otherwise = Node left y (insert x right)

{-
    It gets a list of numbers
    If list is empty, it returns an empty list
    Else it iterates over the elements
-}
createBST :: Ord a => [a] -> BST a
createBST [] = Empty
createBST numbers = create (Node Empty (head numbers) Empty) (tail numbers)
    where 
        create el [] = el
        create el (x:xs) = create (insert x el) xs

{-
    [1,2,3,4,5] = [1] [2,3,4,5]
    [1] [2,3,4,5] = [1 2] [3,4,5]
-}

-- Printing in pre-order
-- Show a puts a constraint on the function that it should be printable obj
preOrder:: Show a => BST a -> IO ()
preOrder Empty = return ()
preOrder (Node left val right) = do
    putStr (show val ++ " ")
    preOrder left
    preOrder right

-- Printing in in-order
-- Show a puts a constraint on the function that it should be printable obj
inOrder:: Show a => BST a -> IO ()
inOrder Empty = return ()
inOrder (Node left val right) = do
    inOrder left
    putStr (show val ++ " ")
    inOrder right

-- Printing in post-order
-- Show a puts a constraint on the function that it should be printable obj
postOrder:: Show a => BST a -> IO ()
postOrder Empty = return ()
postOrder (Node left val right) = do
    postOrder left
    postOrder right
    putStr (show val ++ " ")

--          Part C: Doing a BFS traversal for the BST
-- Here we do the bfs traversal of the bst tree
bfs :: BST a -> [a]
bfs node = bfsHelper [node]
    where 
        bfsHelper [] = []
        bfsHelper (Empty:xs) = bfsHelper xs
        bfsHelper ((Node left val right):xs) = val : bfsHelper (xs ++ [left,right])


main :: IO ()
main = do
    agrs <- getArgs
    if "--test" `elem` agrs
        then testcases
        else interactive

testcases :: IO ()
testcases = do
    let inputs = ["6,3,7,2,4,8,5", "1,2,3,4,5","10,-5,20,0,15"]
  
    putStrLn "Running test cases..."
    mapM_ runTest inputs
    where
    runTest input = do
        putStrLn ("\nInput: " ++ input)
        let parts = parseTheList input
        let numbers = map read parts :: [Int]
        let bst = createBST numbers

        putStr "Pre-Order traversing: "
        preOrder bst
        putStrLn ""
        putStr "In-Order traversing: "
        inOrder bst
        putStrLn ""
        putStr "Post-Order traversing: "
        postOrder bst
        putStrLn ""

        let bfsTraversal = bfs bst
        putStrLn ("BFS traversal: " ++ show bfsTraversal)

interactive :: IO ()
interactive = do
    -- Reading the input
    putStrLn ("Enter the list of comma separated numbers: ")
    input <- getLine

    -- Parsing the input
    let parts = parseTheList input
    let numbers = map read parts :: [Int]

    -- Creating a tree out of it
    let bst = createBST numbers

    -- Printing the BST
    putStr ("Pre-Order traversing: ")
    preOrder(bst)
    putStrLn ""
    putStr ("In-Order traversing: ")
    inOrder(bst)
    putStrLn ""
    putStr ("Post-Order traversing: ")
    postOrder(bst)
    putStrLn ""

    -- Doing BFS traversal for the BST
    let bfsTraversal = bfs bst
    putStr ("BFS traversal: " ++ show bfsTraversal)
    putStrLn ""

    -- print(bst)
    -- putStrLn ("The input is "++ show numbers)