module Main

import Data.Vect

-- let .. in defines local variables
-- where .. allows for local function definitions

-- Nat is a natural number type, non-negative integers.
-- ++ is for appending Strings or Lists to each other.
-- words : String -> List String -- splits on a space
average : (str : String) -> Double
average str =
  let
    numWords = wordCount str
    totalLength = sum (allLengths (words str)) in
    cast totalLength / cast numWords -- this line is the result.
  where
    wordCount : String -> Nat
    wordCount str = length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map length strs

showAverage : String -> String
showAverage str =
  "Average word lenth: " ++
  show (average str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showAverage
-- repl displays a prompt, reads a String from the console and displays the
-- result of running a function on that String, forever.

-- Where an expression can be one of several types, you can declare which
-- inline with the function 'the'
-- the Int 45 yields an Int with the value 45. This is not syntax sugar, 'the'
-- is an ordinary function.
-- the : (a: Type) -> a -> a
-- Given a type, a, and a value of that type, 'the' returns the value. The
-- implementation might look like the type = id -- identity

-- Can partially apply functions
add : Num a => a -> a -> a
add x y = x + y
--
add3 : Int -> Int
add3 = add 3

nine : Int
nine = add3 6

-- <T> T identity(T t) { return t; }
identity : ty -> ty
identity x = x

-- If we want to write a function that can add a number to itself, this won't
-- work:
-- bad_double : ty -> ty
-- bad_double x = x + x
-- "ty is not a numeric type"
-- => means we're setting a restriction on a
double : Num ty => ty -> ty
double x = x + x

-- Defining a function that executes a function twice over a value
twice : (a -> a) -> a -> a
twice f x = f (f x)

quadruple : Num a => a -> a
quadruple x = double (double x)

-- lambda is \arg => result
squared_twice : Integer
squared_twice = twice (\x => x * x) 2 -- 16

-- :t \x => x * x -- Integer -> Integer

-- :t \x : Int, y : Int => x + y -- Int -> Int

-- Tuples are nested pairs
tuples_are_nested_pairs : Bool
tuples_are_nested_pairs = (1, (2, (3, (4)))) == (1, 2, 3, 4)

-- Lists use []. Appending two lists:
one_to_seven : List Integer
one_to_seven = [1, 2, 3] ++ [4, 5, 6, 7]

-- Prepending (consing) to a list is (::)
zero_to_seven : List Integer
zero_to_seven = 0 :: one_to_seven

one_to_four : List Integer
one_to_four = 1 :: 2 :: 3 :: 4 :: []

one_to_a_hundred : List Integer
one_to_a_hundred = [1..100]

-- module Xyz at the top of the file, generally matches Xyz.idr in the filename
-- import Foo looks for Foo.idr in the same directory or elsewhere that Idris
-- can find.

{- Multiline
comment -}

||| Documentation comment for foo
foo : String
-- can be viewed with :doc foo or Ctrl-Alt-D in Atom

-- Exercises
-- 1. Types of
-- ("A", "B", "C") -- (String, String, String)
-- ["A", "B", "C"] -- List String
-- (("A", "B"), "C") -- ((String, String), String)

-- Given a natural number, min, and a String, str, convert str to lowercase and
-- return true if it's the same forwards as backwards, as long as it's over the
-- length 'min'.
palindrome : Nat -> String -> Bool
palindrome min str = if (length str > min) then
    (let lstr = toLower str in reverse lstr == lstr)
  else False

-- (x, y) is a tuple of an x and a y. (Nat, Nat) is a type, two natural numbers.
-- (length (words str), length str) is a value of that type.
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- Ord is an interface something like Java's Comparable.
-- <A extends Comparable<A>> List<A> topTen(List<A> list)
top_ten : Ord a => List a -> List a
top_ten list = take 10 (reverse (sort list))

over_length : Nat -> List String -> Nat
over_length len list = length (filter (\str => length str > len) list)
