import Data.Vect

-- Type synonym
Position : Type
Position = (Double, Double)

-- Type function with pattern matching
StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety-four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x

valToString2 : (isInt : Bool) -> (case isInt of
  False => String
  True => Int) -> String
valToString2 False x = trim x
valToString2 True x = cast x

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : (Num numType) => (numargs : Nat) -> (acc : numType) -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

data Format =
  Number Format | -- %d followed by tail
  Dbl Format |
  Str Format | -- %s followed by tail
  Chr Format |
  Lit String Format | -- Literal string followed by text
  End -- empty

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (s : String) -> PrintfType fmt
PrintfType (Chr fmt) = (c : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (d : Double) -> PrintfType fmt
PrintfType (Lit text fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ cast d)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ cast c)
printfFmt (Lit text fmt) acc = printfFmt fmt (acc ++ text)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: 'c' :: xs) = Chr (toFormat xs)
toFormat ('%' :: 'f' :: xs) = Dbl (toFormat xs)
toFormat ('%' :: xs) = Lit "%" (toFormat xs)
toFormat (x :: xs) = case toFormat xs of
  Lit lit chars => Lit (strCons x lit) chars
  fmt => Lit (strCons x "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

-- Exercise 1 - define a Matrix as a nested vector of Double
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- Exercise 2 is Chr and Dbl above in printf

-- Exercise 3 - implement a vector as nested pairs, with the nesting calculated
-- from the length, e.g.,
-- TupleVect 0 ty = ()
-- TupleVect 1 ty = (ty, ())
-- TupleVect 2 ty = (ty, (ty, ()))
TupleVect : Nat -> Type -> Type
TupleVect Z _ = ()
TupleVect (S k) x = (x, TupleVect k x)
