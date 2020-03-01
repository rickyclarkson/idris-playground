-- IO is what we use to sequence of interactions, a bit like Future in
-- Java, but there's no .get(), and it's not started yet when our function runs.

-- IO is a generic type, so a String -> IO Int function is one that takes a
-- String as an input and gives a description of an interactive program that
-- produces an Int, e.g., readAndGetLength, displays the input as a prompt,
-- reads another String from the console and gives the length of that String.

-- There's notation to make sequencing the operations easier.

module Main

import System
import Data.Vect

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")

-- Equivalent to something like:
main2 : IO ()
main2 =
  putStr "Enter your name: " >>=
  (\_ => getLine >>= (\x =>
    putStrLn ("Hello " ++ x ++ "!")))

-- Or in Java:
-- public static Future<Void> main() {
--   return putStr("Enter your name: ")
--     .flatMap(ignored -> getLine()
--       .flatMap(x -> putStrLn("Hello " + x + "!")));
-- }

-- In the REPL, :c hello produces a binary, hello.

-- >>= (bind, flatMap) works for other types too, not just IO, e.g., List, Vect.
-- That's due to it being defined for Monad or Applicative.

-- Exercise 1 - read two strings and print the length of the longer
printLonger : IO ()
printLonger = do
  putStr "First: "
  first <- getLine
  putStr "second: "
  second <- getLine
  putStrLn ("Max length: " ++ show (if length first > length second then length first else length second))

-- Exercise 2 - same usin >>= instead of do
printLonger2 : IO ()
printLonger2 =
  putStr "First: " >>=
  (\_ => getLine >>=
  (\first => putStr "second: " >>=
  (\_ => getLine >>=
  (\second => putStrLn ("Max length: " ++ show (if length first > length second then length first else length second))))))

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

--readNumbers: IO (Maybe (Nat, Nat))
--readNumbers = do
--  num1 <- readNumber
--  case num1 of
--    Nothing => pure Nothing
--    Just num1_ok => do
--      num2 <- readNumber
--      case num2 of
--        Nothing => pure Nothing
--        Just num2_ok => pure (Just (num1_ok, num2_ok))

readPair : IO (String, String)
readPair = do
  str1 <- getLine
  str2 <- getLine
  pure (str1, str2)

usePair : IO ()
usePair = do
  (str1, str2) <- readPair
  putStrLn ("You entered " ++ str1 ++ " and " ++ str2)

readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  Just num1_ok <- readNumber | Nothing => pure Nothing
  Just num2_ok <- readNumber | Nothing => pure Nothing
  pure (Just (num1_ok, num2_ok))

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S k) = do
  putStrLn (show (S k))
  usleep 1000000
  countdown k

countdowns : IO ()
countdowns = do
  putStr "Enter starting number: "
  Just startNum <- readNumber | Nothing => do
    putStrLn "Invalid input"
    countdowns
  countdown startNum
  putStr "Another (y/n)? "
  yn <- getLine
  if yn == "y" then countdowns else pure ()

-- Exercises 1, 2, 3 - guessing game

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr ("Guess " ++ (show guesses) ++ ": ")
  Just guessed <- readNumber | Nothing => do
    putStrLn "Invalid guess"
    guess target guesses
  if guessed < target then do
    putStrLn "Too low"
    guess target (guesses + 1)
  else if guessed > target then do
    putStrLn "Too high"
    guess target (guesses + 1)
  else do
    putStrLn "That's right!"
    pure ()

guessGame : IO ()
guessGame = do
  x <- time
  guess (cast (mod x 100 + 1)) 1

-- Exercise 4 - own versions of repl and replWith
-- Prelude.Interactive.repl : (prompt : String) ->
--    (onInput : String -> String) -> IO ()
--    A basic read-eval-print loop
--    Arguments:
--        prompt : String  -- the prompt to show

--        onInput : String -> String  -- the function to run on reading input,
--        returning a String to output
intr : (prompt : String) -> (onInput : String -> String) -> IO ()
intr prompt onInput = do
  putStrLn prompt
  input <- getLine
  putStrLn (onInput input)
  intr prompt onInput

-- Prelude.Interactive.replWith : (state : a) ->
--    (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
--    A basic read-eval-print loop, maintaining a state
--    Arguments:
--        state : a  -- the input state

--        prompt : String  -- the prompt to show

--        onInput : a -> String -> Maybe (String, a)  -- the function to run on
--        reading input, returning a String to output and a new state. Returns
--        Nothing if the repl should exit

intrWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
intrWith state prompt onInput = do
  putStrLn prompt
  input <- getLine
  let Just (toOutput, newState) = onInput state input | Nothing => do pure ()
  putStrLn toOutput
  intrWith newState prompt onInput

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  line <- getLine
  xs <- readVectLen k
  pure (line :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (VectUnknown String)
readVect = do
  putStr "Number of elements: "
  Just len <- readNumber | Nothing => do
    putStrLn "Invalid length"
    readVect
  values <- readVectLen len
  pure (MkVect len values)

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

-- dependent pair - a pair where the second value's type is related to the
-- first value.
anyVect : (n : Nat ** Vect n String)
anyVect = (2 ** ["hello", "world"])

readVect2 : IO (n : Nat ** Vect n String)
readVect2 = do
  x <- getLine
  if x == ""
    then pure (_ ** [])
    else do
      (_ ** xs) <- readVect2
      pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do
  (len1 ** vec1) <- readVect2
  (len2 ** vec2) <- readVect2
  case exactLength len1 vec2 of
    Nothing => putStrLn "Vectors are of different lengths"
    Just vec2' => printLn (zip vec1 vec2')

-- Exercise 1 - write readToBlank, reads input until user enters a blank line
readToBlank : IO (List String)
readToBlank = do
  line <- getLine
  if line == ""
    then pure []
    else do
      tail <- readToBlank
      pure (line :: tail)

-- Exercise 2 - write a function to read input until user enters a blank line,
-- then reads a filename and saves it to that file
readToFile : IO ()
readToFile = do
  list <- readToBlank
  filename <- getLine
  Right () <- writeFile filename (show list) | Left err => putStrLn (show err)
  pure ()

-- Exercise 3 - write a function that reads the contents of a file into a
-- dependent pair containing a length and a Vect of that length. Any errors =>
-- empty vector.
readVectHandle : (file : File) -> IO (n ** Vect n String)
readVectHandle file = do
  Right text <- fGetLine file | Left err => pure (_ ** [])
  if text == ""
    then pure (_ ** [])
    else do
      (n ** xs) <- readVectHandle file
      pure (1 + n ** text :: xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right handle <- openFile filename Read | Left err => pure (_ ** [])
  result <- readVectHandle handle
  closeFile handle
  pure result
