# Haskell: Complete Theory and Reference Guide 

## Table of Contents

1. [Introduction & Philosophy](#introduction--philosophy)
2. [Installation & Setup](#installation--setup)
3. [Basic Syntax & Values](#basic-syntax--values)
4. [Types and Type System](#types-and-type-system)
5. [Functions](#functions)
6. [Expressions and Operators](#expressions-and-operators)
7. [Conditionals & Control Flow](#conditionals--control-flow)
8. [Iterations & Recursion](#iterations--recursion)
9. [Lists](#lists)
10. [Tuples](#tuples)
11. [Pattern Matching](#pattern-matching)
12. [Guards](#guards)
13. [Where Clauses](#where-clauses)
14. [List Comprehensions](#list-comprehensions)
15. [Higher-Order Functions (Extended)](#higher-order-functions-extended)
16. [User-Defined Data Types](#user-defined-data-types)
17. [Recursive Types](#recursive-types)
18. [Function Properties & QuickCheck](#function-properties--quickcheck)
19. [Correctness Proofs & Induction](#correctness-proofs--induction)
20. [Tail Recursion & Accumulators](#tail-recursion--accumulators)
21. [Problem Solving with Folds](#problem-solving-with-folds)
22. [Matrices and Nested Lists](#matrices-and-nested-lists)
23. [I/O and Interactive Programs](#io-and-interactive-programs)
24. [Strings & Characters](#strings--characters)
25. [Modules & Imports](#modules--imports)
26. [Lab 3 Solutions](#lab-3-solutions)
27. [Lab 4 Solutions](#lab-4-solutions)
28. [Lab 5 Solutions](#lab-5-solutions)
29. [Quick Reference](#quick-reference)

---

## Introduction & Philosophy

**What is Haskell?**

Haskell is a **purely functional programming language** with strong static typing and lazy evaluation.

**Core Principles:**
- **Pure Functions:** Same input → same output, no side effects
- **Immutability:** Values cannot be changed once created
- **Strong Static Typing:** Compiler catches errors at compile time
- **Lazy Evaluation:** Expressions computed only when needed

---

## Installation & Setup

```bash
ghc --version
ghci              # Start interactive prompt
:load File.hs     # Load a file
:reload           # Reload after changes
:t expression     # Check type
:quit             # Exit
```

---

## Basic Syntax & Values

```haskell
answer = 42
name = "Alice"
greeting = "Hello, " ++ name

-- Comments
{- Multi-line
   comment -}
```

---

## Types and Type System

### Common Types

| Type | Description | Examples |
|------|-------------|----------|
| `Int` | Fixed-size integer | `42`, `-5` |
| `Integer` | Arbitrary precision | Very large numbers |
| `Float`, `Double` | Floating point | `3.14` |
| `Bool` | Boolean | `True`, `False` |
| `Char` | Single character | `'a'` |
| `String` | List of characters | `"hello"` |

### Type Classes

- `Num` – Numeric types
- `Eq` – Equality comparison (`==`, `/=`)
- `Ord` – Ordering (`<`, `>`, `<=`, `>=`)
- `Show` – Convert to string
- `Read` – Parse from string

### Using Read

```haskell
read :: Read a => String -> a

read "5" :: Int           -- 5
read "True" :: Bool       -- True
read "[1,2,3]" :: [Int]   -- [1,2,3]
```

---

## Functions

### Basic Functions

```haskell
square :: Int -> Int
square x = x * x

add :: Int -> Int -> Int
add x y = x + y
```

### Lambda Expressions

```haskell
\x -> x + 1
\x y -> x + y
map (\x -> x^2) [1,2,3]  -- [1,4,9]
```

### Function Composition

```haskell
(f . g) x = f (g x)

double = (* 2)
addOne = (+ 1)
doubleThenAdd = addOne . double
```

### The $ Operator

```haskell
f $ g $ h x  ==  f (g (h x))

-- Avoid parentheses:
sum $ map (^2) [1..10]
```

---

## Conditionals & Control Flow

### If-Then-Else

```haskell
absolute x = if x < 0 then -x else x
```

### Case Expressions

```haskell
describeList xs = case xs of
  []  -> "empty"
  [x] -> "singleton"
  _   -> "longer"
```

---

## Iterations & Recursion

### Basic Recursion

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

### Recursive Variant

For recursive functions, identify the **variant**: a value that decreases with each recursive call until reaching the base case.

Examples:
- `factorial n`: variant is `n`
- `sumList xs`: variant is `length xs`

---

## Lists

### Creating and Accessing

```haskell
[1,2,3,4]
1 : [2,3,4]      -- cons operator
head [1,2,3]     -- 1
tail [1,2,3]     -- [2,3]
last [1,2,3]     -- 3
init [1,2,3]     -- [1,2]
[1..5]           -- [1,2,3,4,5]
```

### Common Functions

```haskell
length, reverse, take, drop, zip, zipWith
maximum, minimum, sum, product
and, or, all, any
concat, replicate
words, unwords  -- String to/from [String]
```

---

## Pattern Matching

```haskell
myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "Empty!"

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y
```

---

## Guards

```haskell
signum' :: Int -> Int
signum' x
  | x < 0     = -1
  | x == 0    = 0
  | otherwise = 1
```

---

## List Comprehensions

```haskell
[x^2 | x <- [1..5]]                -- [1,4,9,16,25]
[x | x <- [1..10], even x]         -- [2,4,6,8,10]
[(x,y) | x <- [1,2], y <- ['a','b']]
```

---

## Higher-Order Functions (Extended)

### Map

```haskell
map :: (a -> b) -> [a] -> [b]
map (+1) [1,2,3]     -- [2,3,4]
map (^2) [1,2,3]     -- [1,4,9]
```

### Filter

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter even [1..10]  -- [2,4,6,8,10]
filter (>0) [-1,0,1] -- [1]
```

### Foldr (Fold Right)

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr (+) 0 [1,2,3]  -- 10
foldr (*) 1 [1,2,3]  -- 6
foldr (&&) True [True, False]  -- False

-- How it works:
foldr f z [x1,x2,x3] = f x1 (f x2 (f x3 z))
```

### Foldl (Fold Left)

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b

foldl (+) 0 [1,2,3]  -- 10
foldl (-) 0 [1,2,3]  -- -6  (0-1-2-3)

-- How it works:
foldl f z [x1,x2,x3] = f (f (f z x1) x2) x3
```

**Key Difference:**
- `foldr`: processes from right, builds result from right
- `foldl`: processes from left, accumulates from left
- Use `foldl` when you need strict left-to-right evaluation
- Use `foldr` for building structures (like lists)

### Foldr1 and Foldl1

Like `foldr`/`foldl` but use first element as initial value:

```haskell
foldr1 max [1,5,3,2]  -- 5
foldl1 (+) [1,2,3]    -- 6
```

⚠️ **Warning:** Crashes on empty list!

### The `twice` Function

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)

twice (+1) 5  -- 7
twice (*2) 3  -- 12
```

### The `iter` Function

```haskell
iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- iter 3 f x = f (f (f x))
```

---

## User-Defined Data Types

### Type Synonyms

```haskell
type String = [Char]
type Pos = (Int, Int)
type Matrix = [[Int]]
```

### Data Declarations

```haskell
data Bool = False | True
data Answer = Yes | No | Unknown

data Shape = Circle Float | Rect Float Float | Ellipse Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
area (Ellipse a b) = pi * a * b
```

### Parameterized Types

```haskell
data Maybe a = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (m `div` n)
```

---

## Recursive Types

### Natural Numbers

```haskell
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
```

### Binary Trees

```haskell
data Tree a = Leaf a | Node (Tree a) a (Tree a)

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
```

---

## Function Properties & QuickCheck

### What is QuickCheck?

QuickCheck is a library for **property-based testing** that automatically generates test cases.

```haskell
import Test.QuickCheck
```

### Writing Properties

```haskell
prop_square :: Integer -> Bool
prop_square x = square x >= 0

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_squares :: Integer -> Integer -> Bool
prop_squares x y = square x + square y + 2*x*y == square (x+y)
```

### Running QuickCheck

```haskell
> quickCheck prop_square
+++ OK, passed 100 tests.
```

### Properties of reverse

```haskell
-- Singleton property
prop_RevUnit :: Int -> Bool
prop_RevUnit x = reverse [x] == [x]

-- Distribution over append
prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- Involution (self-inverse)
prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs
```

---

## Correctness Proofs & Induction

### Pre and Post Conditions

- **Pre-condition:** What must be true about inputs
- **Post-condition:** What will be true about outputs

Example:
```haskell
-- pre: n >= 0
-- post: factorial n = n!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

### Simple Induction

To prove property P for all natural numbers:

1. **Base case:** Prove P(0) or P(1)
2. **Inductive step:** Assume P(n-1), prove P(n)

### Example Proof: sumTo

```haskell
-- pre: n > 0
-- post: sumTo n = 1 + 2 + ... + n
sumTo :: Int -> Int
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)
```

**Proof:**

**Base:** `sumTo 1 = 1 = sum(1 to 1)` ✓

**Inductive:** Assume `sumTo(n-1) = sum(1 to n-1)` [IH]
```
sumTo n = n + sumTo(n-1)       [definition]
        = n + sum(1 to n-1)     [by IH]
        = sum(1 to n)           [definition] ✓
```

### Course of Values Induction

Assume P(m) for all m < n, prove P(n).

Useful when recursive call doesn't decrease by exactly 1.

### Property Proof Example

**Claim:** `sum (doubleAll xs) = 2 * sum xs`

```haskell
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs
```

**Base case:** `xs = []`
```
LHS: sum (doubleAll []) = sum [] = 0
RHS: 2 * sum [] = 0
✓
```

**Inductive case:** Assume property holds for `xs` [IH]
```
LHS: sum (doubleAll (x:xs))
   = sum (2*x : doubleAll xs)
   = 2*x + sum (doubleAll xs)

RHS: 2 * sum (x:xs)
   = 2 * (x + sum xs)
   = 2*x + 2 * sum xs
   = 2*x + sum (doubleAll xs)  [by IH]
   = LHS ✓
```

---

## Tail Recursion & Accumulators

### What is Tail Recursion?

**Tail recursive:** Last operation is the recursive call  
**Non-tail recursive:** Does computation after recursive call

### Why It Matters

Non-tail builds up stack:
```
sumTo 5 = 5 + (4 + (3 + (2 + 1)))  -- 9 stack frames
```

Tail-recursive with accumulator:
```
tailSumTo 5 0 = tailSumTo 4 5 = tailSumTo 3 9 = ... = 15
-- No stack buildup!
```

### Accumulator Pattern

```haskell
-- Non-tail recursive
sumTo :: Int -> Int
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)

-- Tail recursive with accumulator
tailSumTo :: Int -> Int -> Int
tailSumTo 1 acc = 1 + acc
tailSumTo n acc = tailSumTo (n-1) (n + acc)

-- Usage: tailSumTo 5 0
```

### More Examples

**Tail recursive factorial:**
```haskell
factorial :: Int -> Int
factorial n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n-1) (n * acc)
```

**Tail recursive reverse:**
```haskell
reverse :: [a] -> [a]
reverse xs = go xs []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x:acc)
```

---

## Problem Solving with Folds

### When to Use Foldl vs Foldr

**Use `foldr` when:**
- Building a list or data structure
- Need lazy evaluation
- Function associates to the right

**Use `foldl` when:**
- Strict left-to-right processing
- Accumulating a result (sum, product)
- Working with finite lists

### Example: Reverse Polish Notation (RPN)

Evaluate expressions like `"3 8 5 - * 7 +"` (which is `3 * (8-5) + 7 = 16`)

```haskell
evalRPN :: (Num a, Read a) => String -> a
evalRPN = head . foldl procStack [] . words

procStack :: (Num a, Read a) => [a] -> String -> [a]
procStack (x:y:ys) "*" = (y*x) : ys
procStack (x:y:ys) "+" = (y+x) : ys
procStack (x:y:ys) "-" = (y-x) : ys
procStack xs numString = read numString : xs

-- evalRPN "3 8 5 - * 7 +" = 16
```

**How it works:**
1. `words "3 8 5 - * 7 +"` → `["3","8","5","-","*","7","+"]`
2. `foldl` processes left to right, maintaining a stack
3. Numbers: push onto stack
4. Operators: pop two, compute, push result
5. `head` gets final result

---

## Matrices and Nested Lists

### Representing Matrices

```haskell
type Matrix = [[Int]]

-- Example:
-- [[1,4,9],
--  [3,5,7]]
```

### Validating Matrices

A valid matrix has:
1. All rows same length
2. At least one row and one column

```haskell
uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all (== head xs) (tail xs)

valid :: Matrix -> Bool
valid [] = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))
```

### Matrix Operations

**Dimensions:**
```haskell
matrixWidth :: Matrix -> Int
matrixWidth xss = length (head xss)

matrixHeight :: Matrix -> Int
matrixHeight xss = length xss
```

**Matrix Addition:**
```haskell
plusM :: Matrix -> Matrix -> Matrix
plusM m n 
  | ok        = zipWith (zipWith (+)) m n
  | otherwise = error "Invalid dimensions"
  where
    ok = valid m && valid n
         && matrixWidth m == matrixWidth n
         && matrixHeight m == matrixHeight n
```

### Using zipWith

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith (+) [1,2,3] [4,5,6]  -- [5,7,9]
zipWith (*) [1,2] [3,4]      -- [3,8]

-- Nested for matrices:
zipWith (zipWith (+)) [[1,2],[3,4]] [[5,6],[7,8]]
-- [[6,8],[10,12]]
```

---

## I/O and Interactive Programs

### Basic I/O

```haskell
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)
```

### Common I/O Functions

```haskell
putStr :: String -> IO ()      -- Print without newline
putStrLn :: String -> IO ()    -- Print with newline
print :: Show a => a -> IO ()  -- Print any showable value
getLine :: IO String           -- Read a line
getChar :: IO Char             -- Read a character
```

### The `do` Notation

```haskell
main = do
  action1
  result <- action2
  action3 result
```

- Each line is an IO action
- `<-` extracts value from IO action
- Last expression is the return value

### Example: Interactive Calculator

```haskell
calculator :: IO ()
calculator = do
  putStrLn "Enter first number:"
  num1 <- getLine
  putStrLn "Enter second number:"
  num2 <- getLine
  let result = read num1 + read num2
  putStrLn ("Result: " ++ show result)
```

### Example: Game of Nim (Lab 5)

```haskell
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

move :: Board -> Int -> Int -> Board
move board row num = take row board ++ [board !! row - num] ++ drop (row+1) board

playNim :: IO ()
playNim = do
  putStrLn "Game of Nim"
  play initial 1

play :: Board -> Int -> IO ()
play board player
  | finished board = putStrLn ("Player " ++ show (other player) ++ " wins!")
  | otherwise = do
      showBoard board
      putStrLn ("Player " ++ show player ++ "'s turn")
      putStrLn "Which row?"
      row <- getLine
      putStrLn "How many?"
      num <- getLine
      let newBoard = move board (read row) (read num)
      play newBoard (other player)

other :: Int -> Int
other 1 = 2
other 2 = 1

showBoard :: Board -> IO ()
showBoard board = mapM_ showRow (zip [0..] board)

showRow :: (Int, Int) -> IO ()
showRow (row, stars) = putStrLn (show row ++ ": " ++ replicate stars '*')
```

---

## Strings & Characters

```haskell
import Data.Char

toUpper 'a'          -- 'A'
toLower 'A'          -- 'a'
isDigit '5'          -- True
isAlpha 'a'          -- True

map toUpper "hello"  -- "HELLO"
```

---

## Modules & Imports

```haskell
import Data.List
import Data.Char (toUpper, toLower)
import qualified Data.Map as Map
```

---

## Lab 3 Solutions

### 1. mult
```haskell
mult :: Num a => [a] -> a
mult = foldr (*) 1
```

### 2. posList
```haskell
posList :: [Int] -> [Int]
posList = filter (>0)
```

### 3. trueList
```haskell
trueList :: [Bool] -> Bool
trueList = foldr (&&) True
```

### 4. evenList
```haskell
evenList :: [Int] -> Bool
evenList = foldr (\x acc -> even x && acc) True
```

### 5. maxList
```haskell
maxList :: Ord a => [a] -> a
maxList = foldr1 max
```

### 6. inRange
```haskell
inRange :: Int -> Int -> [Int] -> [Int]
inRange low high = filter (\x -> x >= low && x <= high)
```

### 7. countPositives
```haskell
countPositives :: [Int] -> Int
countPositives = length . filter (>0)
```

### 8. myLength
```haskell
myLength :: [a] -> Int
myLength = sum . map (const 1)
```

### 9. myMap
```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []
```

### 10. myLength'
```haskell
myLength' :: [a] -> Int
myLength' = foldr (\_ acc -> acc + 1) 0
```

---

## Lab 4 Solutions

### 1. List Operations

```haskell
squares :: [Int] -> [Int]
squares = map (^2)

sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

allPositive :: [Int] -> Bool
allPositive = all (>0)
```

### 2. Function on Range 0..n

```haskell
minFn :: (Int -> Int) -> Int -> Int
minFn f n = minimum (map f [0..n])

allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = all (== f 0) (map f [0..n])

allFnPositive :: (Int -> Int) -> Int -> Bool
allFnPositive f n = all (>0) (map f [0..n])

increasing :: (Int -> Int) -> Int -> Bool
increasing f n = and [f i <= f (i+1) | i <- [0..n-1]]
```

### 3. Type of twice

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

### 4. iter Function

```haskell
iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)
```

### 5. Power of 2

```haskell
double :: Int -> Int
double x = 2 * x

powerOf2 :: Int -> Int
powerOf2 n = iter n double 1
```

### 6. Blood Type

```haskell
data RhType = Positive | Negative deriving (Show, Eq)
data ABOType = A | B | AB | O deriving (Show, Eq)
data BloodType = BloodType ABOType RhType deriving (Show, Eq)

patient1 = BloodType A Positive
patient2 = BloodType B Negative
patient3 = BloodType AB Positive
patient4 = BloodType O Negative
patient5 = BloodType A Negative

showRh Positive = "+"
showRh Negative = "-"

showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo O _ = True
canDonateTo A A = True
canDonateTo A AB = True
canDonateTo B B = True
canDonateTo B AB = True
canDonateTo AB AB = True
canDonateTo _ _ = False
```

### 7. wonky Function

```haskell
data Answer = Yes | No | Unknown deriving (Show, Eq)

wonky :: Answer -> Answer
wonky Yes = No
wonky No = Unknown
wonky Unknown = Yes

-- Shortest: 3 applications to get back
-- Longest: 3 (it cycles)
```

### 8. Ellipse Shape

```haskell
data Shape = Ellipse Float Float | Rect Float Float deriving Show

circle :: Float -> Shape
circle r = Ellipse r r

area :: Shape -> Float
area (Ellipse a b) = pi * a * b
area (Rect x y) = x * y
```

---

## Lab 5 Solutions

### 1. Factorial with Proof

```haskell
-- pre: n >= 0
-- post: fac n = n!
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- Variant: n (decreases each call)
-- Proof by simple induction on n
```

### 2. Fibonacci with Proof

```haskell
-- pre: n >= 0
-- post: fib n = fibonacci(n)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Variant: n (decreases each call)
-- Proof by course of values induction
```

### 3. Reverse with Proof

```haskell
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- Properties:
-- reverse [x] = [x]
-- reverse (xs ++ ys) = reverse ys ++ reverse xs
-- reverse (reverse xs) = xs

-- Prove by induction on list structure
```

### 4. Nim Game

```haskell
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

move :: Board -> Int -> Int -> Board
move board row num = 
  take row board ++ [board !! row - num] ++ drop (row+1) board

nimGame :: IO ()
nimGame = play initial 1

play :: Board -> Int -> IO ()
play board player
  | finished board = do
      putStrLn ("Player " ++ show (other player) ++ " wins!")
  | otherwise = do
      showBoard board
      putStrLn ("Player " ++ show player)
      putStrLn "Row?"
      row <- getLine
      putStrLn "How many?"
      num <- getLine
      play (move board (read row) (read num)) (other player)

other :: Int -> Int
other 1 = 2
other 2 = 1

showBoard :: Board -> IO ()
showBoard board = sequence_ [showRow i n | (i,n) <- zip [0..] board]

showRow :: Int -> Int -> IO ()
showRow row num = putStrLn (show row ++ ": " ++ replicate num '*')
```

---

## Quick Reference

### Higher-Order Functions

| Function | Type | Purpose |
|----------|------|---------|
| `map` | `(a -> b) -> [a] -> [b]` | Apply function to all |
| `filter` | `(a -> Bool) -> [a] -> [a]` | Keep elements matching predicate |
| `foldr` | `(a -> b -> b) -> b -> [a] -> b` | Fold from right |
| `foldl` | `(b -> a -> b) -> b -> [a] -> b` | Fold from left |
| `zipWith` | `(a -> b -> c) -> [a] -> [b] -> [c]` | Combine two lists element-wise |

### List Functions

```haskell
head, tail, last, init
length, reverse, concat
take, drop, splitAt
zip, unzip, zipWith
maximum, minimum, sum, product
and, or, all, any
replicate, repeat, cycle
```

### I/O Functions

```haskell
putStr, putStrLn, print
getLine, getChar
readFile, writeFile
```

### Useful Patterns

```haskell
-- Point-free style
sumSquares = sum . map (^2)

-- Operator sections
(+1), (*2), (>0), (`div` 2)

-- Function composition
(f . g . h) x = f (g (h x))

-- List comprehensions
[x^2 | x <- [1..10], even x]
```

---

## Summary

This guide covers:

- **Core Haskell:** Types, functions, pattern matching, recursion
- **Higher-Order Functions:** map, filter, foldr, foldl
- **Data Types:** User-defined, recursive, parameterized
- **Function Properties:** QuickCheck, property-based testing
- **Proofs:** Simple and course of values induction
- **Tail Recursion:** Accumulators for efficiency
- **Problem Solving:** RPN evaluation, matrices, nested structures
- **I/O Programming:** Interactive programs, game implementation
- **Complete Lab Solutions:** Labs 3, 4, and 5

---

**Last Updated:** January 2026  
**Status:** Complete Edition v3 with Labs 4 & 5