-- Exercise 2
-- Taylor Griffin

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
