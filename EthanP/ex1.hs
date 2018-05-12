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
                      then rank stack ((r-n)+m)
                      else Nothing
                      where (n,m) = rankC prog
-- Problem b --



------------------- Part 2 ------------------------------------------