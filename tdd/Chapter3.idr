import Data.Vect

-- Defining functions by pattern matching
invert : Bool -> Bool
invert True = False
invert False = True

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail = " ++ show xs

-- Given just the type declaration below, can use Ctrl-Alt-A in Atom to add a
-- definition.
allLengths_list : List String -> List Nat
-- allLengths_list list = ?rhs -- put the cursor on list and do ctrl-alt-c in Atom
-- allLengths_list [] = ?rhs_1
-- allLengths_list (x :: xs) = ?rhs_2
allLengths_list [] = []
allLengths_list (x :: xs) = length x :: allLengths_list xs
-- Check if allLengths_list is total
-- :total allLengths_list -> Main.allLengths_list is Total

allLengths : Vect len String -> Vect len Nat
--allLengths [] = []
--allLengths (word :: words) = length word :: allLengths words
allLengths [] = []
allLengths (word :: words) = 0 :: allLengths words

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = not $ isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = not $ isEven k

fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]

sixInts : Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

insert : Ord elem => (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: ys) = if (x < y) then (x :: y :: ys) else (y :: insert x ys)

insertSort : (Ord elem) => Vect n elem -> Vect n elem
insertSort [] = []
insertSort (x :: xs) = insert x (insertSort xs)

data Matrix : Nat -> Nat -> Type where
  Mat : (c : Nat) -> (r : Nat) -> (items : Vect c (Vect r Int)) -> Matrix c r

items : (Matrix c r) -> Vect c (Vect r Int)
items (Mat c r items) = items

addVect2 : (Vect c (Vect r Int)) -> (Vect c (Vect r Int)) -> (Vect c (Vect r Int))
addVect2 [] ys = []
addVect2 (x :: xs) (y :: ys) = map (\(left, right) => left + right) (zip x y) :: addVect2 xs ys

addMatrix : (Matrix c r) -> (Matrix c r) -> (Matrix c r)
addMatrix (Mat c r xs) (Mat c r ys) = Mat c r (addVect2 xs ys)

-- dummy change to test vc
