module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items) where
  addToData : Vect old String -> Vect (S old) String
  addToData [] = [newItem]
  addToData (x :: xs) = x :: addToData xs

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp =
  let val = cast inp in
    if val < 0
      then Nothing
      else let newVal = tot + val in
        Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command =
  Add String |
  Get Integer |
  Search String |
  Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
  False => Nothing
  True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "search" substr = Just (Search substr)
parseCommand _ _ = Nothing

parse : String -> Maybe Command
parse input = case span (/= ' ') input of
  (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
  case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    Just id => Just (index id store_items ++ "\n", store)

searchData : (substr : String) -> (items : Vect size String) -> String -> Int -> String
searchData substr [] acc index = acc
searchData substr (x :: xs) acc index = case Strings.isInfixOf substr x of
  False => searchData substr xs acc (index + 1)
  True => "" ++ show index ++ ": " ++ x ++ "\n" ++ searchData substr xs acc (index + 1)

searchEntry : (substr : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry substr store@(MkData size items) = Just (searchData substr items "" 0, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
  Nothing => Just ("Invalid command\n", store)
  Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (Get pos) => getEntry pos store
  Just (Search substr) => searchEntry substr store
  Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

-- Exercise 1 is size above, not sure why it's in the exercises.
-- Exercise 2 - searchEntry above
