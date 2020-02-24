import Data.Vect

-- Data type definition

-- Enumeration
data LightSwitch = On | Off

toggle : LightSwitch -> LightSwitch
toggle On = Off
toggle Off = On

-- Union, basically an email where members can have values
data Shape =
  Triangle Double Double |
  Rectangle Double Double |
  Circle Double

-- Recursive
data Nat2 = Z2 | S Nat2

-- List looks like
-- data List elem = Nil | (::) elem (List elem)

data Tree elem =
  Empty |
  Node (Tree elem) elem (Tree elem)
%name Tree tree, tree2

-- inserting into a binary search tree, not caring about balancing
-- Ord elem => means restrict elem to types that there is an Ord for, so
-- we can compare two values.
insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x original@(Node left y right) = case compare x y of
  LT => Node (insert x left) y right
  EQ => original
  GT => Node left y (insert x right)

-- Exercise 1
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- Exercise 2
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

-- *Chapter4> treeToList (listToTree [4,1,8,7,2,3,5,9,6])
-- [1, 2, 3, 4, 5, 6, 7, 8, 9] : List Integer

-- Exercise 3
data Expr =
  EInt Integer |
  EAdd Expr Expr |
  ESub Expr Expr |
  EMul Expr Expr

-- Exercise 4
evaluate : Expr -> Integer
evaluate (EInt x) = x
evaluate (EAdd x y) = evaluate x + evaluate y
evaluate (ESub x y) = evaluate x - evaluate y
evaluate (EMul x y) = evaluate x * evaluate y

-- Exercise 5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

-- Exercise 6
-- Biggest triangle's area.
shapeArea : Shape -> Double
shapeArea (Triangle base height) = 0.5 * base * height
shapeArea (Rectangle x y) = x * y
shapeArea (Circle radius) = pi * radius * radius

data Picture =
  Primitive Shape |
  Combine Picture Picture |
  Rotate Double Picture |
  Translate Double Double Picture

pictureArea : Picture -> Double
pictureArea (Primitive shape) = shapeArea shape
pictureArea (Combine x y) = pictureArea x + pictureArea y
pictureArea (Rotate angle picture) = pictureArea picture
pictureArea (Translate x y picture) = pictureArea picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape@(Triangle x y)) = Just (shapeArea shape)
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate angle picture) = biggestTriangle picture
biggestTriangle (Translate x y picture) = biggestTriangle picture

-- Dependent Types
-- From Data.Vect
-- data Vect : Nat -> Type -> Type
-- A list whose length is in its type.

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : (wheels : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels Motorcycle = 2
wheels (Tram wheels) = wheels



refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible -- compile time check that something wouldn't compile
refuel (Motorcycle fuel) = Motorcycle 50


append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip2 : Vect n a -> Vect n b -> Vect n (a, b)
zip2 [] [] = []
zip2 (x :: xs) (y :: ys) = (x, y) :: zip xs ys

index2 : Fin n -> Vect n elem -> elem
index2 FZ (x :: xs) = x
index2 (FS k) (x :: xs) = index2 k xs

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} index vect = case integerToFin index n of
  Nothing => Nothing
  Just x => Just (index2 x vect)

-- Exercise 3 and 4 (1 and 2 are embedded above)
vectTake : (n : Nat) -> Vect (n + m) elem -> Vect n elem
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

-- Exercise 5
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
  Nothing => Nothing
  Just index => Just (index2 index xs + index2 index ys)
