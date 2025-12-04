# Haskell: Complete Theory and Reference Guide (Extended Edition v2)

A comprehensive guide covering all essential Haskell concepts from fundamentals through advanced topics, organized by theme for easy reference. Includes extended sections on Higher-Order Functions, User-Defined Data Types, Function Properties, QuickCheck Testing, and Correctness Proofs.

---

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
21. [Strings & Characters](#strings--characters)
22. [Modules & Imports](#modules--imports)
23. [I/O Basics](#io-basics)
24. [Practical Examples](#practical-examples)
25. [Lab 3 Exercise Solutions](#lab-3-exercise-solutions)
26. [Lab 4 Exercise Solutions](#lab-4-exercise-solutions)
27. [Quick Reference Table](#quick-reference-table)

---

## Introduction & Philosophy

**What is Haskell?**

Haskell is a **purely functional programming language** with strong static typing and lazy evaluation. Unlike imperative languages (Python, C, Java), Haskell treats programs as expressions that evaluate to values, rather than sequences of statements that modify state.

**Core Principles:**

- **Pure Functions:** Functions always return the same output for the same input. No side effects.
- **Immutability:** Once a value is created, it cannot be changed.
- **Strong Static Typing:** The compiler checks types at compile time.
- **Lazy Evaluation:** Expressions are only computed when needed.
- **No Variable Reassignment:** Variables are bound to values once.

---

## Installation & Setup

### Installing Haskell

1. **Download GHC:** Visit [https://www.haskell.org/downloads/](https://www.haskell.org/downloads/)
2. **Verify Installation:**
   ```bash
   ghc --version
   ghci
   ```

### Using GHCi

- **Start GHCi:** `ghci`
- **Load a file:** `:load MyFile.hs` or `:l MyFile.hs`
- **Reload after changes:** `:reload` or `:r`
- **Check type:** `:t expression`
- **Quit:** `:quit` or `:q`

---

## Basic Syntax & Values

```haskell
answer = 42
name = "Alice"
greeting = "Hello, " ++ name

-- Single-line comment
{- Multi-line comment -}
```

---

## Types and Type System

### Common Types

| Type | Description | Examples |
|------|-------------|----------|
| `Int` | Fixed-size integer | `42`, `-5` |
| `Integer` | Arbitrary precision integer | `123456789012345` |
| `Float` | Single-precision floating point | `3.14` |
| `Double` | Double-precision floating point | `3.141592653589793` |
| `Bool` | Boolean values | `True`, `False` |
| `Char` | Single character | `'a'`, `'Z'` |
| `String` | List of characters | `"hello"` |

### Type Classes

- `Num` – numeric types
- `Eq` – types with equality (`==`, `/=`)
- `Ord` – ordered types (`<`, `>`, `<=`, `>=`)
- `Show` – types convertible to strings

### Polymorphic Types

```haskell
length :: [a] -> Int
max :: Ord a => a -> a -> a
```

---

## Functions

### Defining Functions

```haskell
square :: Int -> Int
square x = x * x

add :: Int -> Int -> Int
add x y = x + y
```

### Currying & Partial Application

```haskell
add5 = add 5    -- partially applied
add5 3          -- returns 8
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
addThenDouble = double . addOne
addThenDouble 5  -- 12
```

### The `$` Operator

```haskell
f $ g $ h x  ==  f (g (h x))
sum $ map (^2) [1..10]
```

---

## Expressions and Operators

```haskell
-- Arithmetic
3 + 5, 3 - 5, 3 * 5, 10 / 2
10 `div` 3, 10 `mod` 3, 2 ^ 3

-- Comparison
5 == 5, 5 /= 3, 3 < 5, 3 > 5

-- Logical
True && False, True || False, not True
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

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)

sumList [] = 0
sumList (x:xs) = x + sumList xs
```

---

## Lists

### Creating and Accessing

```haskell
[1,2,3,4]
1 : [2,3,4]     -- cons operator
head [1,2,3]    -- 1
tail [1,2,3]    -- [2,3]
[1..5]          -- [1,2,3,4,5]
```

### Common Functions

```haskell
length, reverse, take, drop, zip
maximum, minimum, sum, product
and, or, all, any
```

---

## Tuples

```haskell
pair = (1, "hello")
fst pair    -- 1
snd pair    -- "hello"
```

---

## Pattern Matching

```haskell
myHead (x:_) = x
myHead [] = error "Empty!"

sumPair (x, y) = x + y
```

---

## Guards

```haskell
signum' x
  | x < 0     = -1
  | x == 0    = 0
  | otherwise = 1
```

---

## Where Clauses

```haskell
bmi weight height
  | bmiVal < 18.5 = "Underweight"
  | bmiVal < 25.0 = "Normal"
  | otherwise     = "Overweight"
  where
    bmiVal = weight / height ^ 2
```

---

## List Comprehensions

```haskell
[x^2 | x <- [1..5]]                    -- [1,4,9,16,25]
[x | x <- [1..10], even x]             -- [2,4,6,8,10]
[(x,y) | x <- [1,2], y <- ['a','b']]   -- Cartesian product
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

### Foldr

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (+) 0 [1,2,3,4]     -- 10
foldr (*) 1 [1,2,3,4]     -- 24
foldr (&&) True [True, False]  -- False
```

**How foldr works:**
```
foldr f z [x1,x2,x3] = f x1 (f x2 (f x3 z))
```

### Foldr1 (No Initial Value)

```haskell
foldr1 max [1,5,3,2]  -- 5
```

### The `twice` Function

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- or: twice f = f . f

twice (+1) 5  -- 7
twice (*2) 3  -- 12
```

### The `iter` Function (Repeated Application)

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
type Pair a = (a, a)
```

### Data Declarations

```haskell
data Bool = False | True
data Answer = Yes | No | Unknown

data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
```

### Parameterized Types

```haskell
data Maybe a = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
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

QuickCheck is a library for **automated property-based testing**. Instead of writing individual test cases, you write **properties** that your functions should satisfy, and QuickCheck generates random test cases to verify them.

### Setting Up QuickCheck

```haskell
import Test.QuickCheck
```

### Writing Properties

Properties are functions that return `Bool`:

```haskell
prop_square :: Integer -> Bool
prop_square x = square x >= 0

prop_squares :: Integer -> Integer -> Bool
prop_squares x y = square x + square y + 2*x*y == square (x+y)
```

### Properties of `reverse`

```haskell
-- reverse of singleton is itself
prop_RevUnit :: Int -> Bool
prop_RevUnit x = reverse [x] == [x]

-- reverse distributes over ++
prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- reverse is its own inverse
prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs
```

### Running QuickCheck

In GHCi:
```haskell
> quickCheck prop_square
+++ OK, passed 100 tests.

> quickCheck prop_RevRev
+++ OK, passed 100 tests.
```

### Example: Crossword Helper

```haskell
-- Using list comprehension
cwordFind :: Char -> Int -> Int -> [String] -> [String]
cwordFind letter pos len words = 
  [wd | wd <- words, length wd == len, wd !! pos == letter]

-- Using recursion
cwordFindRec :: Char -> Int -> Int -> [String] -> [String]
cwordFindRec letter pos len [] = []
cwordFindRec letter pos len (x:xs) =
  if (x !! pos == letter) && (length x == len) 
  then x : cwordFindRec letter pos len xs
  else cwordFindRec letter pos len xs

-- Using higher-order functions
cwordFindHO :: Char -> Int -> Int -> [String] -> [String]
cwordFindHO letter pos len words = filter p words
  where p x = (x !! pos == letter) && (length x == len)

-- Property: all implementations should be equivalent
prop_cwfCompRec letter pos len words =
  cwordFind letter pos len words == cwordFindRec letter pos len words
```

---

## Correctness Proofs & Induction

### Pre and Post Conditions

- **Pre-condition:** Logical statement that holds on input data
- **Post-condition:** Logical statement that holds after function application

### Simple Induction

To show property P holds for all natural numbers:
1. **Base case:** Show P(0) or P(1)
2. **Inductive step:** Assume P(n-1), prove P(n)

### Example: sumTo

```haskell
-- pre: n > 0
-- post: sumTo n = 1 + 2 + ... + n
sumTo :: Int -> Int
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)
```

**Proof by induction:**

**Base case (n = 1):**
```
sumTo 1 = 1 = sum(1 to 1) ✓
```

**Inductive step:**
Assume sumTo(n-1) = sum(1 to n-1) [IH]

```
sumTo(n) = n + sumTo(n-1)     [definition]
         = n + sum(1 to n-1)   [by IH]
         = sum(1 to n)         [definition of sum] ✓
```

### Course of Values Induction

To show P holds: if P(m) for all m < N, then P(N).

### Example: mult

```haskell
-- pre: x, y >= 0
-- post: mult x y = x * y
mult :: Int -> Int -> Int
mult x 0 = 0
mult x y 
  | y `mod` 2 == 0 = 2 * mult x (y `div` 2)
  | otherwise      = x + 2 * mult x (y `div` 2)
```

### Property Proof: sum (doubleAll xs) = 2 * sum xs

```haskell
sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

-- QuickCheck property
prop_SumDoubleAll :: [Integer] -> Bool
prop_SumDoubleAll xs = sum (doubleAll xs) == 2 * sum xs
```

**Proof:**

**Base case (xs = []):**
```
LHS: sum (doubleAll []) = sum [] = 0
RHS: 2 * sum [] = 2 * 0 = 0
LHS = RHS ✓
```

**Inductive case:**
Assume: sum (doubleAll xs) = 2 * sum xs [IH]

```
LHS: sum (doubleAll (x:xs))
   = sum (2*x : doubleAll xs)    [def doubleAll]
   = 2*x + sum (doubleAll xs)    [def sum]

RHS: 2 * sum (x:xs)
   = 2 * (x + sum xs)            [def sum]
   = 2*x + 2 * sum xs            [arithmetic]
   = 2*x + sum (doubleAll xs)    [by IH]
   = LHS ✓
```

### Property: length (xs ++ ys) = length xs + length ys

```haskell
prop_lengthPlusPlus :: [Int] -> [Int] -> Bool
prop_lengthPlusPlus xs ys = length (xs ++ ys) == length xs + length ys
```

---

## Tail Recursion & Accumulators

### What is Tail Recursion?

A **tail recursive** function calls itself directly as the last operation. **Non-tail recursive** functions do computation with the recursive result.

| Function | Type |
|----------|------|
| `remainder` | Tail recursive |
| `sumTo` | Non-tail recursive |
| `mult` | Non-tail recursive |

### Why Tail Recursion Matters

**Non-tail recursive** builds up stack frames:
```
sumTo 5 = 5 + sumTo 4
        = 5 + (4 + sumTo 3)
        = 5 + (4 + (3 + sumTo 2))
        = 5 + (4 + (3 + (2 + sumTo 1)))
        = 5 + (4 + (3 + (2 + 1)))
        -- 9 stack operations!
```

**Tail recursive** with accumulator:
```
tailSumTo 5 0 = tailSumTo 4 5
              = tailSumTo 3 9
              = tailSumTo 2 12
              = tailSumTo 1 14
              = 15
              -- No stack buildup!
```

### What is an Accumulator?

An **accumulator** is an extra parameter that accumulates the answer as recursion proceeds. The accumulator gets bigger as the variant gets smaller.

### Converting to Tail Recursion

**Non-tail recursive:**
```haskell
sumTo :: Int -> Int
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)
```

**Tail recursive with accumulator:**
```haskell
-- pre: x >= 1, acc >= 0
-- post: tailSumTo x acc = sum(1 to x) + acc
tailSumTo :: Int -> Int -> Int
tailSumTo 1 acc = 1 + acc
tailSumTo x acc = tailSumTo (x - 1) (x + acc)

-- Usage: tailSumTo 5 0 = 15
```

### More Examples

**Tail recursive length:**
```haskell
lengthTail :: [a] -> Int
lengthTail xs = go xs 0
  where
    go [] acc = acc
    go (_:xs) acc = go xs (acc + 1)
```

**Tail recursive reverse:**
```haskell
reverseTail :: [a] -> [a]
reverseTail xs = go xs []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x:acc)
```

**Tail recursive factorial:**
```haskell
factorialTail :: Int -> Int
factorialTail n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n-1) (n * acc)
```

---

## Strings & Characters

```haskell
import Data.Char (toUpper, toLower)

map toUpper "hello"  -- "HELLO"
head "hello"         -- 'h'
```

---

## Modules & Imports

```haskell
import Data.List
import Data.Char (toUpper, toLower)
import qualified Data.Map as Map
```

---

## I/O Basics

```haskell
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)
```

---

## Practical Examples

### Squares List
```haskell
squares :: [Int] -> [Int]
squares ns = map (^2) ns
-- or: squares = map (^2)
```

### Sum of Squares
```haskell
sumSquares :: [Int] -> Int
sumSquares ns = sum (map (^2) ns)
-- or: sumSquares = sum . map (^2)
```

### All Positive
```haskell
allPositive :: [Int] -> Bool
allPositive ns = all (>0) ns
-- or: allPositive = all (>0)
```

---

## Lab 3 Exercise Solutions

### 1. mult - Product of a list
```haskell
mult :: Num a => [a] -> a
mult = foldr (*) 1
```

### 2. posList - Return only positive integers
```haskell
posList :: [Int] -> [Int]
posList = filter (>0)
```

### 3. trueList - Check if all Booleans are True
```haskell
trueList :: [Bool] -> Bool
trueList = foldr (&&) True
```

### 4. evenList - Check if all numbers are even
```haskell
evenList :: [Int] -> Bool
evenList = foldr (\x acc -> even x && acc) True
```

### 5. maxList - Maximum of a list
```haskell
maxList :: Ord a => [a] -> a
maxList = foldr1 max
```

### 6. inRange - Numbers within a range
```haskell
inRange :: Int -> Int -> [Int] -> [Int]
inRange low high = filter (\x -> x >= low && x <= high)
```

### 7. countPositives
```haskell
countPositives :: [Int] -> Int
countPositives = length . filter (>0)
```

### 8. myLength - Using foldr and map
```haskell
myLength :: [a] -> Int
myLength = sum . map (const 1)
```

### 9. myMap - Define map using foldr
```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []
```

### 10. myLength' - Using foldr only
```haskell
myLength' :: [a] -> Int
myLength' = foldr (\_ acc -> acc + 1) 0
```

---

## Lab 4 Exercise Solutions

### 1. List of Integers Functions

**Return squares:**
```haskell
squares :: [Int] -> [Int]
squares ns = map (^2) ns
```

**Sum of squares:**
```haskell
sumSquares :: [Int] -> Int
sumSquares ns = sum (map (^2) ns)
-- or: sumSquares = sum . squares
```

**All greater than zero:**
```haskell
allPositive :: [Int] -> Bool
allPositive ns = all (>0) ns
```

### 2. Functions on Function Values (0 to n)

**Minimum value of f on 0 to n:**
```haskell
minFn :: (Int -> Int) -> Int -> Int
minFn f n = minimum (map f [0..n])
```

**Test if all values of f are equal:**
```haskell
allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = all (== f 0) (map f [0..n])
```

**Test if all values are positive:**
```haskell
allFnPositive :: (Int -> Int) -> Int -> Bool
allFnPositive f n = all (>0) (map f [0..n])
```

**Check if values are in increasing order:**
```haskell
increasing :: (Int -> Int) -> Int -> Bool
increasing f n = and [f i <= f (i+1) | i <- [0..n-1]]
-- Alternative using pairs:
increasing f n = all (\(a,b) -> a <= b) (zip vals (tail vals))
  where vals = map f [0..n]
```

### 3. Type of `twice`

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- or: twice f = f . f
```

### 4. The `iter` Function

```haskell
iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- Alternative (tail recursive):
iter n f x = go n x
  where
    go 0 acc = acc
    go n acc = go (n-1) (f acc)
```

### 5. Power of 2 Using `iter`

```haskell
double :: Int -> Int
double x = 2 * x

powerOf2 :: Int -> Int
powerOf2 n = iter n double 1

-- powerOf2 3 = iter 3 double 1 = double (double (double 1)) = 8
```

### 6. Blood Type Modeling

```haskell
-- Define RhType
data RhType = Positive | Negative
  deriving (Show, Eq)

-- Define ABOType
data ABOType = A | B | AB | O
  deriving (Show, Eq)

-- Combine into BloodType
data BloodType = BloodType ABOType RhType
  deriving (Show, Eq)

-- Create 5 patients
patient1 :: BloodType
patient1 = BloodType A Positive

patient2 :: BloodType
patient2 = BloodType B Negative

patient3 :: BloodType
patient3 = BloodType AB Positive

patient4 :: BloodType
patient4 = BloodType O Negative

patient5 :: BloodType
patient5 = BloodType A Negative

-- Display functions
showRh :: RhType -> String
showRh Positive = "+"
showRh Negative = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- Donation rules (ABO only, simplified)
canDonateTo :: ABOType -> ABOType -> Bool
canDonateTo O _  = True           -- O can donate to anybody
canDonateTo A A  = True
canDonateTo A AB = True
canDonateTo B B  = True
canDonateTo B AB = True
canDonateTo AB AB = True
canDonateTo _ _  = False

-- Full blood type donation (including Rh)
canDonateToFull :: BloodType -> BloodType -> Bool
canDonateToFull (BloodType abo1 rh1) (BloodType abo2 rh2) =
  canDonateTo abo1 abo2 && canDonateRh rh1 rh2
  where
    canDonateRh Negative _ = True
    canDonateRh Positive Positive = True
    canDonateRh Positive Negative = False
```

### 7. Answer Type - `wonky` Function

```haskell
data Answer = Yes | No | Unknown
  deriving (Show, Eq)

-- One possible implementation
wonky :: Answer -> Answer
wonky Yes = No
wonky No = Unknown
wonky Unknown = Yes

-- With this wonky:
-- wonky Yes = No (wrong)
-- wonky (wonky Yes) = wonky No = Unknown (wrong)
-- wonky (wonky (wonky Yes)) = wonky Unknown = Yes (correct!)
-- Shortest: 3 applications, Longest: 3 applications (cycles back)

-- Alternative implementation
wonky2 :: Answer -> Answer
wonky2 Yes = Unknown
wonky2 No = Yes
wonky2 Unknown = No
-- This also cycles with period 3
```

### 8. Ellipse Shape

```haskell
data Shape = Ellipse Float Float | Rect Float Float
  deriving Show

-- Circle as a special case of Ellipse (equal radii)
circle :: Float -> Shape
circle r = Ellipse r r

-- Area function
area :: Shape -> Float
area (Ellipse a b) = pi * a * b    -- for ellipse
area (Rect x y) = x * y

-- Test:
-- area (circle 5) = pi * 5 * 5 = 78.54...
-- area (Ellipse 3 4) = pi * 3 * 4 = 37.70...
```

---

## Quick Reference Table

| Concept | Example | Purpose |
|---------|---------|---------|
| Value binding | `x = 42` | Define a constant |
| Type annotation | `f :: Int -> Int` | Declare function type |
| Function def | `f x = x + 1` | Define a function |
| Lambda | `\x -> x + 1` | Anonymous function |
| Function composition | `f . g` | Compose functions |
| List creation | `[1,2,3]` | Create a list |
| List comprehension | `[x^2 \| x <- [1..5]]` | Generate list |
| Pattern matching | `f (x:xs) = ...` | Unpack values |
| Guard | `f x \| x > 0 = ...` | Conditional logic |
| Where clause | `f x = a where a = ...` | Local definitions |
| Map | `map (*2) [1,2,3]` | Apply function to all |
| Filter | `filter even [1..10]` | Select elements |
| Foldr | `foldr (+) 0 [1,2,3]` | Accumulate |
| Type synonym | `type Pos = (Int, Int)` | Alias for existing type |
| Data type | `data Bool = True \| False` | New type with constructors |
| QuickCheck prop | `prop_f x = f x == ...` | Property-based test |
| Tail recursion | `go xs acc = go xs' (f acc)` | Efficient recursion |

---

## Summary

This extended guide covers:

- **Core Haskell:** Types, functions, pattern matching, guards, recursion
- **Higher-Order Functions:** `map`, `filter`, `foldr`, `foldl`, `twice`, `iter`
- **User-Defined Types:** `type` synonyms, `data` declarations, recursive types
- **Function Properties:** QuickCheck testing, writing and verifying properties
- **Correctness Proofs:** Simple induction, course of values induction
- **Tail Recursion:** Accumulators, converting non-tail to tail recursive
- **Lab 3 & 4 Solutions:** Complete solutions with explanations

---

**Last Updated:** December 2025  
**Status:** Extended Edition v2 with QuickCheck, Proofs, and Lab 4