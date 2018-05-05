type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int

type rank = Int
type stackRank = (Int,Int)

ranking :: Cmd -> stackRank
ranking LD = (0,1)
ranking ADD = (2,1)
ranking MULT = (2,1)
ranking DUP = (1,2)
ranking INC = (1,1)
ranking SWAP = (2,2)
ranking (POP k) = (k,0)
