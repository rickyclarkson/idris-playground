-- IO is what we use to sequence of interactions, a bit like Future in
-- Java, but there's no .get(), and it's not started yet when our function runs.

-- IO is a generic type, so a String -> IO Int function is one that takes a
-- String as an input and gives a description of an interactive program that
-- produces an Int, e.g., readAndGetLength, displays the input as a prompt,
-- reads another String from the console and gives the length of that String.

-- There's notation to make sequencing the operations easier.

module Main

import System -- for usleep and time

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
