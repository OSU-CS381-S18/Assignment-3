-- Exercise 3

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

