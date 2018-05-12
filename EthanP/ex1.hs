------------------- Part 1 ------------------------------------------
-- Problem a --
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC -- Increments the topmost element on the stack.
         | SWAP -- Exchanges the two topmost elements on the stack.
         | POP Int -- k pops k elements of the stack.

type Rank = Int -- Used to rank a single element.

-- The rank of a single stack operation is
-- given by a pair of numbers (n, m).
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank -- Maps each stack operation to its rank.

rankC (LD k) = (0,1) -- Loads one k int onto the stack
rankC ADD = (2,1) -- Takes two off the stack and puts their sum back onto the stack. 
rankC MULT = (2,1) -- Takes two off the stack and puts their product back onto the stack.
rankC DUP = (1,2) -- Takes one off the stack copys it and puts the copy and original back onto the stack.
rankC INC = (1,1) -- Takes the first element off the stack and increments it, then puts it back on top.
rankC SWAP = (2,2) -- Swaps the two topmost elements and puts them back on the stack.
rankC (POP k) = (k,0) -- Pops k number of elements of the stack.

rankP :: Prog -> Maybe Rank -- Computes the rank of a program
rankP [] = Just 0
rankP prog = rank prog 0

rank :: Prog -> Rank -> Maybe Rank -- Auxiliary function
rank [] r = Just r
rank (prog:stack) r = if n <= r
                      then rank stack ((r-n)+m) -- ((r-n)+m) calculates the rank current Rank r - the expected number of elements the
                      else Nothing --                operation takes n + the number of elements the opperation puts back onto the stack m. 
                      where (n,m) = rankC prog--     --Example: Let current stack size be 3 = r Add(2,1) n = 2 and m = 2 ((3-2)+1) = 2 the new stack rank/size.
-- Problem b --
-- Reusing some code from HW2
type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

sem :: Prog -> D
sem [] stack = stack

sem (x:xs) (Just c) = if (sem xs(semCmd x (Just c))) == Nothing
                      then Nothing
                      else (sem xs(semCmd x (Just c)))

semCmd :: Cmd -> D
semCmd ADD(Just list) = if length list == 0
                        then Nothing
                        else if length list == 1
                        then Nothing
                        else Just (((head list) + (head (tail list))) : drop 2 list)

semCmd MULT(Just list) = if length list == 0
                       then Nothing 
                       else if length list == 1
                       then Nothing
                       else Just (((head list) * (head (tail list))) : drop 2 list)

semCmd DUP(Just list) = if length list == 0
                       then Nothing 
                       else Just ((list !! 0) : list)                     


typeCorrect ::  Prog -> Bool
typeCorrect prog = rankP prog /= Nothing

semStatTC :: Prog -> Maybe(Maybe Stack)
semStatTC prog | typeCorrect prog = Just (sem prog (Just([])))
               | otherwise = Nothing
                
------------------- Part 2 ------------------------------------------