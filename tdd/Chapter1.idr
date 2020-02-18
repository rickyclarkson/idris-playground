module Main

-- Vect is a typed list, the first type argument is the number of elements, the second is the type of them.
-- x -> y means 'given an x, yields a y'
-- x -> y -> z means 'given an x and a y, yields a z', something like BiFunction<X, Y, Z> in Java or (X, Y) => Z in Scala, etc.
-- Suggest operations for the following types:
--
-- 1) Vect n elem -> Vect n elem -- identity/id
-- 2) Vect n elem -> Vect (n * 2) elem -- append/(++)
-- 3) Vect (1 + n) elem -> Vect n elem -- remove first element
-- Assume Bounded n is a number between 0 and n - 1,
-- Bounded n -> Vect n elem -> elem -- get at index

-- Function calls don't need parentheses, e.g., sin x rather than sin(x).

-- Functions are pure, no observable side effects. Idris is strict by default.
-- Real programs are possible, similar to Java methods that return
-- ListenableFuture<>, we set up a chain of actions but never actually wait on a
-- ListenableFuture.
-- Idris' IO is similar to ListenableFuture but there is no way of waiting.
-- Sounds slow but it isn't. IO String ~= ListenableFuture<String>.

example_integer : Integer
example_integer = 6 + 8 * 11 -- 94

example_double : Double
example_double = 2.1 * 20

reversing_a_string : String
reversing_a_string = reverse "abcdefg"

appending_strings : String
appending_strings = "Hello" ++ " " ++ "World!"

-- At the REPL, begun by executing idris with no command line args:
-- Idris> :t 2 + 2
-- 2 + 2 : Integer -- not 4 : Integer, :t doesn't execute anything.

-- Idris> 2 + 2
-- 4 : Integer

-- Idris> :t Type
-- Type : Type 1 -- The type of simple types is Type 1, the type of Type 1 is
-- Type 2, etc., but we only ever need to write Type.

-- Example program, needs module Main which is at the top of this file.
-- idris Chapter1.idr
-- Idris> :exec

main : IO ()
main = putStrLn "Hello, Idris World!"

-- Or idris Chapter1.idr -o Hello
-- ./Hello

-- If we don't know what a value should be yet we can add a hole, e.g.:
-- main = putStrLn ?greeting
-- This will compile and give a warning, but can be executed.

-- Can use this to help understand more complex types, e.g., when we want to
-- putStrLn a Char instead of a String
-- main = putStrLn (?convert 'x')
-- Chapter1> :t convert
-- convert : Char -> String
-- can replace convert with cast.
-- main = putStrLn (cast 'x')

-- Types are values. x = Int is fine.

-- Calculating a type given a Boolean as a input:
-- StringOrInt is a function, the convention is to capitalize functions that
-- return Type.
StringOrInt : Bool -> Type
StringOrInt x = case x of
  True => Int
  False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
  True -> 94
  False => "Ninety four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of
  True => cast val
  False => val
