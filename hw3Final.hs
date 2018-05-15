{--
=================================HW3================================

Contributors :: Ethan Patterson | Taylor Griffin | Lucien Tamno
				| Blake Hudson | Ethan Ahuja 
--}


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
                      where (n,m) = rankC prog--     --Example: Let current stack size be 3 = r Add(2,1) n = 2 and m = 1 ((3-2)+1) = 2 the new stack rank/size.
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
semCmd (LD x) (Just list) = (Just (x:list))
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
                       
semCmd INC(Just list) = if length list == 0
                      then Nothing
                      else Just(((head list) + 1) : drop 1 list)
                      
semCmd SWAP (Just (x:y:list)) = if length list == 0
                              then Nothing
                              else Just(y:x:list)
semCmd (POP k) (Just list) = if length list == 0 || (length list) < k
                      then Nothing
                      else Just(drop k list)

typeCorrect ::  Prog -> Bool
typeCorrect prog = rankP prog /= Nothing

semStatTC :: Prog -> Maybe(Maybe Stack)
semStatTC prog | typeCorrect prog = Just (sem prog (Just([])))
               | otherwise = Nothing

p :: Prog
p = [LD 3,DUP,ADD,DUP,MULT,LD 5, LD 4, INC, SWAP, POP 1]

q :: Prog
q = [LD 3,ADD]
e = []

empty = Just []
test1 = sem p empty
test2 = sem q empty
test3 = sem e empty 
                
------------------- Part 2 ------------------------------------------

data Shape = X
  | TD Shape Shape
  | LR Shape Shape
  deriving Show

type BBox = (Int,Int)

-- a)

bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2) =
  if (s1x > s2x) then (s1x,sy)
  else (s2x,sy)
  where
    (s1x,s1y) = bbox s1
    (s2x,s2y) = bbox s2
    sy = s1y + s2y
bbox (LR s1 s2) =
  if (s1y > s2y) then (sx,s1y)
  else (sx,s2y)
  where
    (s1x,s1y) = bbox s1
    (s2x,s2y) = bbox s2
    sx = s1x + s2x

-- b)

rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD s1 s2) = case rect s1 of
  Nothing -> Nothing
  Just (s1x,s1y) -> case rect s2 of
    Nothing -> Nothing
    Just (s2x,s2y) -> if (s1x == s2x) then Just (s1x,s1y+s2y) else Nothing
rect (LR s1 s2) = case rect s1 of
  Nothing -> Nothing
  Just (s1x,s1y) -> case rect s2 of
    Nothing -> Nothing
    Just (s2x,s2y) -> if (s1y == s2y) then Just (s1x+s2x,s1y) else Nothing

-- Testing

-- rectangles
s0 = LR X X
s1 = TD X X
s2 = LR (s1) (s1)
s3 = TD (s0) (s0)
-- non rectangle
s4 = LR (s0) (s1)
-- Testing part a
test_a_0 = bbox s0
test_a_1 = bbox s1
test_a_2 = bbox s2
test_a_3 = bbox s3
-- Testing part b
test_b_0 = rect s0
test_b_1 = rect s1
test_b_2 = rect s2
test_b_3 = rect s3
test_b_4 = rect s4

------------------- Part 3 ------------------------------------------

{--
a) Consider the functions f and g, which are given by the
following two function definitions
f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]
g [] y = []
1)  f :: [a] -> a -> [a]
    g :: [a] -> b -> [b]
2) Function f returns either [y] or x. Assuming f is type correct,
x must be of the same type as [y]. Therefore, f takes [expr] and
expr, and returns [expr].
Function g returns either [y] or []. There's no scenario where x is
returned, so we can effectively ignore it and assume its of different
type than y. Given this, we can say g takes [expr1] and expr2, and
returns [expr2], because if it returns an empty list than can still
be assumed to be an empty list of type expr2.
3) Function g is more general because it is less restrictive for the
second argument (argument x has to be a list but it can be a list of
any type).
4) The two functions are of different types for the reason specified
in the previous question, f has a condition for the second argument
that g does not.
--}

--Part B)
--h :: [b] -> [(a, b)] -> [b]

h x y = if not (null x) then map snd y else x

--Part C)
--k :: (a -> b) -> ((a -> b) -> a) -> b 

k m n = m ( n (m))

--Part D)
{--
No this does not work. The operation (a -> b) means in words is given any type a, 
it will return any type b thats not of type a. Since Haskell is a strongly typed 
language, it can not change the type of a to any other type using polymorphism. 
Polymorphism takes a type and returns the same type. But that type may be any type.
--}