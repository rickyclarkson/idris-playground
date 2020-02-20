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
-- allLengths_list list = ?rhs -- put the cursor on list and do ctrl-alt-c in
-- Atom to expand the cases as below:
-- allLengths_list [] = ?rhs_1
-- allLengths_list (x :: xs) = ?rhs_2
allLengths_list [] = []
allLengths_list (x :: xs) = length x :: allLengths_list xs
-- Check if allLengths_list is total
-- :total allLengths_list -> Main.allLengths_list is Total

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = 0 :: allLengths words

-- :doc List pulls up the documentation for List.
-- In Atom, ctrl-alt-D

-- Data types are defined in terms of their constructors, a bit like Java enum
-- members but often with parameters.

-- mutual lets you define functions that call each other, otherwise Idris
-- requires everything to be top-down.

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

my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S (my_length xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

-- total makes sure it won't compile if it's not total, total meaning every
-- scenario is accounted for.

-- I struggled with this, something about two-dimensional arrays seems to cause
-- me problems. Transposing means taking the top row, transposing the rest
-- recursively, then prefixing each transposed row with the values from the top
-- row to form a new column.

-- Ctrl-Alt-T in Atom to find types, only works on the right hand side.
-- Ctrl-Alt-A to add missing cases
-- Ctrl-Alt-C to split out a case by destructuring a particular variable
-- Ctrl-Alt-S to do an expression search, if the type is specific enough it
-- can fill in a right hand side.

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

prefixEach : (x : Vect n elem) -> Vect n (Vect k elem) -> Vect n (Vect (S k) elem)
prefixEach [] [] = []
prefixEach (x :: xs) (y :: ys) = (x :: y) :: prefixEach xs ys

total transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = prefixEach x (transposeMat xs)

-- Exercise 1 - reimplement using zipWith

total transposeMat_zipWith : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat_zipWith [] = replicate _ []
transposeMat_zipWith (x :: xs) = zipWith (::) x (transposeMat_zipWith xs)
