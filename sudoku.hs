import Data.List
import Data.Maybe

type Sudoku = [[Int]]
type Board = [[[Int]]]


puzzle :: [[Int]]
puzzle= [[2,8,9,0,0,0,0,0,4],[0,0,3,4,0,0,0,8,0],[0,4,0,0,0,2,0,0,0],
     [0,6,0,0,0,0,1,5,2],[0,0,0,0,6,0,0,0,0],[8,3,1,0,0,0,0,7,0],
     [0,0,0,1,0,0,0,3,0],[0,5,0,0,0,4,9,0,0],[1,0,0,0,0,0,8,4,7]]

-- Turns a Sudoku input into a board of possible choices for each square
makeBoard :: Sudoku -> Board
makeBoard = map (map (\x -> if (x==0) then [1..9] else [x]))



-- Returns a row of a matrix
getRow :: Int -> Board -> [[Int]]
getRow r b = b !! r



-- Returns a given Column of a matrix
getCol :: Int -> Board -> [[Int]]
getCol r b = transpose b !! r



-- Set functions to help build the boxes
set :: Int -> [a] -> [a]
set 0 [a,b,c,_,_,_,_,_,_] = [a,b,c]
set 1 [_,_,_,d,e,f,_,_,_] = [d,e,f]
set 2 [_,_,_,_,_,_,g,h,i] = [g,h,i]



-- Returns a box corresponding to a number 1-8
box :: Int -> Board -> [[Int]]
box n = concat . map (set c) . (set r)
  where r = n `div` 3
        c = n `mod` 3



-- Converts the board into boxes and returns the board in box form
boxes :: Board -> Board
boxes b = [box i b | i <- [0..8]]


-- Checks if there are duplicate numbers in a row/column/box
noDuplicates :: [[Int]] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = not (elem x xs) && noDuplicates xs



-- Checks if singletons in a row/column/box are duplicates 
noSingleDuplicates :: [[Int]] -> Bool
noSingleDuplicates = noDuplicates . filter ((== 1) . length)



-- Checks the whole board for conflicting singles
validSingles :: Board -> Bool
validSingles b = foldr (&&) True (map noSingleDuplicates b ++ map noSingleDuplicates (transpose b) ++
                map noSingleDuplicates (map (\x -> box x b) [0..3]))



-- Checks if a puzzle solution is valid
valid :: Board -> Bool
valid b = foldr (&&) True (map noDuplicates b ++ map noDuplicates (transpose b) ++ map noDuplicates (map (\x -> box x b) [0..3]))



-- Returns all squares that contain a single choice for the row/column/box passed in
getSingles :: [[Int]] -> [Int]
getSingles = concat . filter ((==1) . length)



-- Removes choices returned from getSingles from squares in the appropriate
-- row/column/box
remove :: [Int] -> [[Int]] -> [[Int]]
remove xs = map (\x -> if length x == 1 then x else filter (`notElem` xs) x)



-- Determines if a board is possible
possible :: Board -> Bool
possible b = (all (all (/=[])) b) && (validSingles b)



-- checks if board is solved
solved :: Board -> Bool
solved b = (all (all ((==1) . length)) b) && (valid b)



-- Reduces all rows, boxes, and columns, and returns a reduced board
reduce :: Board -> Board
reduce = (boxes . reduce' . boxes) . (transpose . reduce' . transpose) . reduce'



-- Reduces the possibilities of each square in each row of a board
reduce' :: Board -> Board
reduce' b = zipWith remove (map getSingles b) b



-- Calls reduce until the board can't be reduced further
fullReduce :: Board -> Board
fullReduce b = if b == (reduce b) then b else fullReduce (reduce b)



-- Checks if a board is possible and solved.  If possible and not solved, continue.
-- Otherwise, stop solution
checkBoard :: Board -> Maybe Board
checkBoard b
  | solved x = Just x
  | not (possible x) = Nothing
  | h == True = y
  | h == False = checkBoard (getTail x)
    where x = fullReduce b
          h = isJust y
          y = checkBoard (getHead x)



-- Finds the first item in the puzzle that satisfies a list longer than 1
findElem :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
findElem p f [[]] = [[]]
findElem p f ([]:xs) = []:findElem p f xs
findElem p f ((x:xs):xss) = if p x then ((f x):xs):xss else (x:y):ys
  where y:ys = (findElem p f (xs:xss))



-- Returns the board with the first item that has length greater than 1 replaced with the head of that list
getHead :: Board -> Board
getHead = findElem (\x -> length x /= 1) (\x -> [head x])



-- Returns the board with the first item that has length greater than 1 replaced with the tail of that list
getTail :: Board -> Board
getTail = findElem (\x -> length x /= 1) (\x -> tail x)



-- Solves the board
solve :: Sudoku -> Sudoku
solve b = map concat (fromJust (checkBoard (makeBoard b)))



main :: IO()
main = do
        print(solve puzzle)