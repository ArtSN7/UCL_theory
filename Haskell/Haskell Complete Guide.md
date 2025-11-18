# Haskell: Complete Theory and Reference Guide

A comprehensive guide covering all essential Haskell concepts from fundamentals through intermediate topics, organized by theme for easy reference.

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
15. [Higher-Order Functions](#higher-order-functions)
16. [Strings & Characters](#strings--characters)
17. [Modules & Imports](#modules--imports)
18. [I/O Basics](#io-basics)
19. [Practical Examples](#practical-examples)
20. [Quick Reference Table](#quick-reference-table)

---

## Introduction & Philosophy

**What is Haskell?**

Haskell is a **purely functional programming language** with strong static typing and lazy evaluation. Unlike imperative languages (Python, C, Java), Haskell treats programs as expressions that evaluate to values, rather than sequences of statements that modify state.

**Core Principles:**

- **Pure Functions:** Functions always return the same output for the same input. No side effects (no hidden modifications to global state).
- **Immutability:** Once a value is created, it cannot be changed. Instead of modifying data, you create new values.
- **Strong Static Typing:** The compiler checks types at compile time, catching many errors before runtime.
- **Lazy Evaluation:** Expressions are only computed when their results are needed, enabling efficient use of memory and infinite data structures.
- **No Variable Reassignment:** Variables are bound to values once; they never "change."

**Why This Matters:**

These principles make Haskell code:
- **Predictable:** No hidden state changes make code easier to reason about.
- **Safe:** Many common bugs (null pointer exceptions, race conditions) are caught by the type system.
- **Composable:** Pure functions are easy to combine and reuse.

---

## Installation & Setup

### Installing Haskell

1. **Download GHC (Glasgow Haskell Compiler):**
   - Visit [https://www.haskell.org/downloads/](https://www.haskell.org/downloads/)
   - Choose your operating system and download the installer

2. **Verify Installation:**
   Open a terminal and run:
   ```bash
   ghc --version
   ghci
   ```
   You should see the GHCi prompt: `Prelude>`

### Using GHCi (The Interactive Prompt)

- **Start GHCi:** Type `ghci` in your terminal
- **Load a file:** `:load MyFile.hs` or `:l MyFile.hs`
- **Reload after changes:** `:reload` or `:r`
- **Check type:** `:t expression`
- **Get help:** `:?`
- **Quit:** `:quit` or `:q`

### Writing Haskell Programs

- Save your code in a `.hs` file (e.g., `MyProgram.hs`)
- Load in GHCi or run with: `runhaskell MyProgram.hs`

---

## Basic Syntax & Values

### Defining Values

```haskell
answer = 42
name = "Alice"
greeting = "Hello, " ++ name
pi_approx = 3.14159
```

- Values are defined with `=` (not assignment; it's binding)
- No semicolons required at the end of lines
- **Indentation matters** (like Python)

### Comments

```haskell
-- This is a single-line comment

{- This is a 
   multi-line comment -}
```

### First Program: Hello World

```haskell
main = putStrLn "Hello, World!"
```

Run with:
```bash
runhaskell hello.hs
```

Or load in GHCi and type:
```haskell
> main
```

---

## Types and Type System

### Type Annotations

Haskell infers types automatically, but you should always annotate function types:

```haskell
answer :: Int
answer = 42

name :: String
name = "Alice"

-- Function type: parameter types -> return type
add :: Int -> Int -> Int
add x y = x + y
```

The `::` operator reads as "has type."

### Common Types

| Type | Description | Examples |
|------|-------------|----------|
| `Int` | Fixed-size integer (32 or 64 bits) | `42`, `-5`, `0` |
| `Integer` | Arbitrary precision integer | `123456789012345` |
| `Float` | Single-precision floating point | `3.14`, `2.71` |
| `Double` | Double-precision floating point | `3.141592653589793` |
| `Bool` | Boolean values | `True`, `False` |
| `Char` | Single character | `'a'`, `'Z'`, `'1'` |
| `String` | String (list of characters) | `"hello"`, `""` |

### Checking Types

In GHCi:
```haskell
> :t 42
42 :: Num a => a
> :t "hello"
"hello" :: String
> :t True
True :: Bool
```

### Type Classes

Haskell uses type classes to group types that support certain operations:

- `Num` – numeric types (Int, Integer, Float, Double)
- `Eq` – types that support equality (`==`, `/=`)
- `Ord` – types that can be ordered (`<`, `>`, `<=`, `>=`)
- `Show` – types that can be converted to strings

---

## Functions

### Defining Simple Functions

```haskell
square :: Int -> Int
square x = x * x
```

- Type signature: `square` takes an `Int`, returns an `Int`
- Function body: `x * x`
- Parameter `x` is used directly (no parentheses)

### Calling Functions

```haskell
square 5        -- returns 25
```

No parentheses or commas between arguments.

### Multi-Argument Functions

```haskell
add :: Int -> Int -> Int
add x y = x + y

sumSquares :: Int -> Int -> Int
sumSquares x y = x^2 + y^2
```

In the type signature, arrows separate parameters and the return type. The rightmost type is always the return type.

### Currying

Every Haskell function technically takes one argument and returns a value. Multi-argument functions are built via **currying**:

```haskell
add :: Int -> Int -> Int
add x = \y -> x + y  -- equivalent to the definition above
```

This means you can partially apply functions:

```haskell
add5 = add 5  -- a function that adds 5 to its argument
add5 3        -- returns 8
```

### Lambda Expressions (Anonymous Functions)

```haskell
\x -> x + 1          -- adds 1 to its input
\x -> x * x          -- squares its input
\x y -> x + y        -- adds two numbers
```

Lambda functions are useful with higher-order functions:

```haskell
map (\x -> x^2) [1,2,3]  -- returns [1,4,9]
```

### Function Composition

Combine functions with the `(.)` operator:

```haskell
double x = x * 2
addOne x = x + 1

addThenDouble = double . addOne  -- apply addOne, then double
addThenDouble 5  -- (5 + 1) * 2 = 12
```

---

## Expressions and Operators

### Arithmetic Operators

```haskell
3 + 5       -- 8 (addition)
3 - 5       -- -2 (subtraction)
3 * 5       -- 15 (multiplication)
10 / 2      -- 5.0 (division)
10 `div` 3  -- 3 (integer division)
10 `mod` 3  -- 1 (remainder/modulo)
2 ^ 3       -- 8 (exponentiation)
```

### Comparison Operators

```haskell
5 == 5      -- True
5 /= 3      -- True (not equal)
3 < 5       -- True
3 > 5       -- False
3 <= 3      -- True
5 >= 4      -- True
```

### Logical Operators

```haskell
True && False   -- False (AND)
True || False   -- True (OR)
not True        -- False (NOT)
```

### String Operations

```haskell
"Hello" ++ " World"   -- "Hello World" (concatenation)
"abc" ++ "def"        -- "abcdef"
```

### Order of Operations

Haskell follows standard mathematical precedence:

1. Function application (highest precedence)
2. `^`, `*`, `/`, `div`, `mod`
3. `+`, `-`
4. `==`, `/=`, `<`, `>`, `<=`, `>=`
5. `&&`
6. `||` (lowest precedence)

Use parentheses to override: `(3 + 4) * 2 == 14`

---

## Conditionals & Control Flow

### If-Then-Else Expressions

In Haskell, `if-then-else` is an **expression** (returns a value), not a statement.

```haskell
if condition then value1 else value2
```

**Key points:**
- Both branches are **required**
- Both branches must return the **same type**
- The condition must be a `Bool`

**Example:**

```haskell
absolute :: Int -> Int
absolute x = if x < 0 then -x else x

max3 :: Int -> Int -> Int -> Int
max3 a b c = if a > b 
             then if a > c then a else c
             else if b > c then b else c
```

**Nested if-then-else** gets hard to read. Use **guards** instead.

### Case Expressions

Case expressions allow pattern matching on values:

```haskell
case expression of
  pattern1 -> result1
  pattern2 -> result2
  pattern3 -> result3
```

**Example:**

```haskell
evaluateGrade :: Char -> String
evaluateGrade grade = case grade of
  'A' -> "Excellent"
  'B' -> "Good"
  'C' -> "Fair"
  'D' -> "Poor"
  'F' -> "Fail"
  _   -> "Invalid grade"

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
  []  -> "empty."
  [x] -> "a singleton."
  xs  -> "longer."
```

---

## Iterations & Recursion

### Recursion: The Haskell Loop

Haskell has no `for` or `while` loops. Instead, use **recursion** to repeat computations.

**Basic Pattern:**

```haskell
baseCase = result
recursiveCase = ... recursiveCall ...
```

**Example: Counting Down**

```haskell
countdown :: Int -> [Int]
countdown 0 = [0]
countdown n = n : countdown (n - 1)

-- countdown 5 => [5,4,3,2,1,0]
```

**Example: Sum a List**

```haskell
sumList :: [Int] -> Int
sumList [] = 0                 -- base case: empty list sums to 0
sumList (x:xs) = x + sumList xs  -- recursive case: add first, recurse on rest
```

**Example: Factorial**

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**Example: Length of a List**

```haskell
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
```

### Tail Recursion (Efficiency)

For large inputs, deep recursion can cause stack overflow. Use **tail recursion with accumulators**:

```haskell
-- Non-tail recursive (can overflow)
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Tail recursive with accumulator
sumListTail :: [Int] -> Int
sumListTail xs = go xs 0
  where
    go [] acc = acc
    go (x:xs) acc = go xs (acc + x)
```

The compiler can optimize tail recursion, avoiding stack overflow.

### Higher-Order Functions for Iteration

Instead of explicit loops, use built-in functions:

**map** – Transform each element:
```haskell
map (^2) [1..5]        -- [1,4,9,16,25]
map toLower "HELLO"    -- "hello"
```

**filter** – Select elements:
```haskell
filter even [1..10]    -- [2,4,6,8,10]
filter (>0) [-2,-1,0,1,2]  -- [1,2]
```

**foldr / foldl** – Accumulate results:
```haskell
foldr (+) 0 [1,2,3,4]  -- 10
foldl (*) 1 [1,2,3,4]  -- 24
```

### List Comprehensions

Generate and filter lists declaratively:

```haskell
[x^2 | x <- [1..10]]                    -- [1,4,9,...,100]
[x | x <- [1..20], even x]              -- [2,4,6,...,20]
[x^2 | x <- [1..10], even x]            -- [4,16,36,64,100]
[(x,y) | x <- [1,2], y <- ['a','b']]   -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

---

## Lists

### Creating Lists

```haskell
numbers = [1, 2, 3, 4]
letters = ['a', 'b', 'c']
emptyList = []
stringList = ["hello", "world"]
```

### Building Lists with Cons

The cons operator `:` prepends an element:

```haskell
1 : [2, 3, 4]  -- [1, 2, 3, 4]
'a' : "bc"     -- "abc"
x : xs         -- prepend x to list xs
```

Every list is built this way: `[1,2,3]` is `1 : 2 : 3 : []`

### Accessing List Elements

```haskell
head [1,2,3]    -- 1 (first element)
tail [1,2,3]    -- [2,3] (all but first)
last [1,2,3]    -- 3 (last element)
init [1,2,3]    -- [1,2] (all but last)
[1,2,3] !! 1    -- 2 (access by index)
length [1,2,3]  -- 3
null []         -- True
null [1]        -- False
```

**Warning:** `head []`, `tail []`, `last []`, `init []` cause runtime errors. Use pattern matching instead.

### Concatenating Lists

```haskell
[1,2] ++ [3,4]        -- [1,2,3,4]
"hello" ++ " world"   -- "hello world"
concat [[1,2],[3,4]]  -- [1,2,3,4]
```

### Ranges

```haskell
[1..5]       -- [1,2,3,4,5]
[1,3..10]    -- [1,3,5,7,9] (step by 2)
[10,9..1]    -- [10,9,8,7,6,5,4,3,2,1] (descending)
[1..]        -- [1,2,3,4,...] (infinite list)
```

### Common List Functions

```haskell
reverse [1,2,3]          -- [3,2,1]
sort [3,1,2]             -- [1,2,3]
take 3 [1..10]           -- [1,2,3]
drop 2 [1..5]            -- [3,4,5]
zip [1,2,3] ['a','b','c']  -- [(1,'a'),(2,'b'),(3,'c')]
```

### List Comprehensions for Advanced Operations

```haskell
-- Transform and filter
[x*2 | x <- [1..5], x > 2]  -- [6,8,10]

-- Multiple generators
[(x,y) | x <- [1,2], y <- [3,4]]  -- [(1,3),(1,4),(2,3),(2,4)]

-- Nested comprehensions
[[x*y | y <- [1..3]] | x <- [1..3]]  -- [[1,2,3],[2,4,6],[3,6,9]]
```

---

## Tuples

### Creating Tuples

Tuples group a fixed number of values, possibly of different types:

```haskell
pair = (1, "hello")
triple = (True, 2.5, 'a')
quadruple = (1, 2, 3, 4)
```

**Tuples vs Lists:**
- Tuples have **fixed length**; lists are flexible
- Tuples can have **different element types**; lists are homogenous
- Tuples are accessed by position; lists are typically pattern-matched

### Accessing Tuple Elements

```haskell
fst (1, "hello")    -- 1
snd (1, "hello")    -- "hello"

-- For larger tuples, use pattern matching:
(a, b, c) = (1, 2, 3)
a                   -- 1
```

### Using Tuples

```haskell
-- Return multiple values
divmod :: Int -> Int -> (Int, Int)
divmod a b = (a `div` b, a `mod` b)

-- Zip combines lists into tuples
zip [1,2,3] ['a','b','c']  -- [(1,'a'),(2,'b'),(3,'c')]
```

---

## Pattern Matching

Pattern matching allows you to unpack structures and branch based on them.

### Patterns in Function Definitions

```haskell
-- Match on list structure
myHead :: [a] -> a
myHead (x:_) = x              -- matches any list with at least one element
myHead [] = error "Empty!"    -- matches empty list

-- Match multiple elements
mySecond :: [a] -> a
mySecond (_:x:_) = x

-- Match tuples
sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

-- Match literals
greet :: String -> String
greet "Alice" = "Hi Alice!"
greet "Bob" = "Hello Bob!"
greet name = "Hi " ++ name
```

### Wildcards and Ignoring Values

```haskell
-- _ means "I don't care about this value"
myHead :: [a] -> a
myHead (x:_) = x

getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x
```

### Nested Patterns

```haskell
processTriple :: (String, Int, [Int]) -> String
processTriple (name, age, []) = name ++ " has no scores"
processTriple (name, age, x:xs) = name ++ " has " ++ show (x:xs)
```

---

## Guards

Guards provide cleaner conditional logic than nested `if-then-else`.

### Basic Syntax

```haskell
function parameters
  | condition1 = result1
  | condition2 = result2
  | otherwise  = defaultResult
```

- Guards are evaluated **top to bottom**
- First matching guard is used
- `otherwise` is syntactic sugar for `True` (catch-all)

### Examples

**Simple Guard:**

```haskell
signum' :: Int -> Int
signum' x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1
```

**Multiple Conditions:**

```haskell
gradeScore :: Int -> String
gradeScore score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise = "F"
```

**Using Functions in Guards:**

```haskell
classify :: Int -> String
classify x
  | even x = "Even"
  | otherwise = "Odd"
```

**Guard with Pattern Matching:**

```haskell
sumList :: [Int] -> Int
sumList xs
  | null xs = 0
  | otherwise = head xs + sumList (tail xs)
```

### Comparison: Guards vs If-Then-Else

| Aspect | Guards | If-Then-Else |
|--------|--------|--------------|
| Readability | Excellent for multiple conditions | Poor when nested |
| Scope | Visible to all guards in function | Local to each branch |
| Use Case | Multi-way conditional | Simple binary choice |

---

## Where Clauses

The `where` clause lets you define local variables/functions visible only within that declaration.

### Basic Usage

```haskell
sumSquares :: Int -> Int -> Int
sumSquares x y = squareX + squareY
  where
    squareX = x ^ 2
    squareY = y ^ 2
```

### With Guards

Shared values across guards:

```haskell
bmi :: Double -> Double -> String
bmi weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise = "Obese"
  where
    bmi = weight / height ^ 2
```

### Multiple Definitions

```haskell
describeTriple :: Int -> Int -> Int -> String
describeTriple a b c = result
  where
    sum = a + b + c
    product = a * b * c
    average = sum `div` 3
    result = "Sum: " ++ show sum ++ ", Avg: " ++ show average
```

### Pattern Matching in Where

```haskell
getCoordinates :: (Double, Double) -> String
getCoordinates pair = "X: " ++ show x ++ ", Y: " ++ show y
  where
    (x, y) = pair
```

### Indentation Rules

- `where` must be indented more than the function line
- All bindings in the `where` block must align

```haskell
-- Correct
func x = a + b
  where
    a = x + 1
    b = x * 2

-- Incorrect (where at same level as func)
func x = a + b
where
  a = x + 1
  b = x * 2
```

---

## List Comprehensions

List comprehensions provide a declarative way to generate and filter lists, inspired by mathematical set notation.

### Basic Syntax

```haskell
[expression | generator, predicate]
```

- **expression:** What to include in the result
- **generator:** Declaration `variable <- list`
- **predicate:** Filter condition (optional)

### Simple Examples

**Generate squares:**

```haskell
[x^2 | x <- [1..5]]  -- [1,4,9,16,25]
```

**Generate and filter:**

```haskell
[x | x <- [1..10], even x]  -- [2,4,6,8,10]
[x^2 | x <- [1..10], x > 5]  -- [36,49,64,81,100]
```

### Multiple Generators

Create Cartesian products:

```haskell
[(x, y) | x <- [1,2], y <- ['a','b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

### Multiple Predicates

All conditions must be true:

```haskell
[x | x <- [1..20], even x, x > 10]  -- [12,14,16,18,20]
```

### Dependent Generators

Later generators can depend on earlier ones:

```haskell
[(x, y) | x <- [1..3], y <- [x..3]]
-- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
```

### Nested Comprehensions

```haskell
-- Remove odds from each sublist
removeOdds = [[x | x <- xs, even x] | xs <- [[1,2,3],[2,4,6],[5,7,8]]]
-- [[2],[2,4,6],[8]]
```

### Complex Transformations

```haskell
-- Pairs of elements that sum to 10
pairs = [(x, y) | x <- [1..9], y <- [1..9], x + y == 10, x < y]
-- [(1,9),(2,8),(3,7),(4,6),(5,5)]
```

### Comprehension with Functions

```haskell
labels = [show x ++ "!" | x <- [1..5]]
-- ["1!","2!","3!","4!","5!"]
```

---

## Higher-Order Functions

Higher-order functions take functions as arguments or return functions.

### Map

Apply a function to every element:

```haskell
map :: (a -> b) -> [a] -> [b]

map (+1) [1,2,3]         -- [2,3,4]
map (^2) [1,2,3]         -- [1,4,9]
map toLower "HELLO"      -- "hello"
```

### Filter

Select elements that satisfy a condition:

```haskell
filter :: (a -> Bool) -> [a] -> [a]

filter even [1..10]      -- [2,4,6,8,10]
filter (>5) [1..10]      -- [6,7,8,9,10]
filter (>0) [-2,-1,0,1,2]  -- [1,2]
```

### Fold (Reduce)

Accumulate elements into a single value:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b

sum [1,2,3,4]            -- 10 (foldr (+) 0)
product [1,2,3,4]        -- 24 (foldr (*) 1)
foldr (++) "" ["a","b"]  -- "ab"
```

### Custom Higher-Order Functions

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice (+1) 5  -- 7
applyTwice (*2) 3  -- 12
```

### Map with Conditional

Transform only elements meeting a condition:

```haskell
-- Keep all, transform only evens
map (\x -> if even x then x^2 else x) [1..5]  -- [1,4,3,16,5]

-- Using list comprehension
[if even x then x^2 else x | x <- [1..5]]
```

### Combining Map and Filter

```haskell
-- Filter, then map
map (^2) (filter even [1..10])  -- [4,16,36,64,100]

-- Equivalent list comprehension
[x^2 | x <- [1..10], even x]
```

---

## Strings & Characters

### What is a String?

A `String` is a list of characters: `String = [Char]`

```haskell
"hello" == ['h','e','l','l','o']  -- True
```

### Characters vs Strings

```haskell
'h'       -- Char (single character, single quotes)
"h"       -- String (string, double quotes)
"hello"   -- String (list of chars)
['h','e','l','l','o']  -- String
```

### String Operations

```haskell
length "hello"        -- 5
"He" ++ "llo"         -- "Hello" (concatenation)
head "hello"          -- 'h'
tail "hello"          -- "ello"
take 3 "hello"        -- "hel"
drop 2 "hello"        -- "llo"
reverse "hello"       -- "olleh"
```

### Iterating Over Strings

Since strings are lists, use all list functions:

```haskell
map toUpper "hello"               -- "HELLO"
map toLower "HELLO"               -- "hello"
filter (/= 'a') "banana"         -- "bnn"
[c | c <- "hello", c /= 'l']     -- "heo"
```

### Case Conversion

Import `Data.Char`:

```haskell
import Data.Char (toUpper, toLower)

toUpper 'a'          -- 'A'
toLower 'A'          -- 'a'
map toUpper "hello"  -- "HELLO"
```

### Accessing First Character

```haskell
head "hello"             -- 'h'
"hello" !! 0             -- 'h'

-- Safe access (pattern matching)
firstChar :: String -> Char
firstChar (c:_) = c
firstChar [] = error "Empty!"

-- Safe with Maybe
safeFirstChar :: String -> Maybe Char
safeFirstChar (c:_) = Just c
safeFirstChar [] = Nothing
```

### Converting Between Lists and Strings

**List of Characters to String:**

```haskell
['h','e','l','l','o']  -- "hello" (already a string)
```

**List of Numbers to String:**

```haskell
concatMap show [1,2,3]          -- "123"
map show [1,2,3]                -- ["1","2","3"]
intercalate "," (map show [1,2,3])  -- "1,2,3"

-- List comprehension
concat [show x | x <- [1,2,3]]  -- "123"
```

**Single Character to String:**

```haskell
[c]        -- wraps character in a list
'h' : ""   -- also works
```

### String Formatting with Show

```haskell
show 42                    -- "42"
show 3.14                  -- "3.14"
show True                  -- "True"
show [1,2,3]              -- "[1,2,3]"
```

---

## Modules & Imports

Modules help organize code and provide reusable functionality.

### Importing Modules

```haskell
import Data.List           -- import all functions from Data.List
import Data.Char           -- import all functions from Data.Char
import Data.List (sort, nub)  -- import specific functions
import qualified Data.Map as Map  -- qualify to avoid conflicts
```

### Common Modules

- `Data.List` – list operations (sort, group, nub, etc.)
- `Data.Char` – character operations (toUpper, toLower, isDigit, etc.)
- `Data.Maybe` – Maybe type utilities
- `Text.Printf` – formatted printing

### Using Imported Functions

```haskell
import Data.List (sort)
sort [3,1,2]  -- [1,2,3]

import Data.Char (toUpper)
map toUpper "hello"  -- "HELLO"
```

### Qualified Imports

```haskell
import qualified Data.Map as Map
Map.fromList [(1, "a"), (2, "b")]
```

### Creating Your Own Module

```haskell
module MyModule where

square x = x * x
double x = x * 2
```

Then import:

```haskell
import MyModule
square 5  -- 25
```

---

## I/O Basics

### The Do Notation

Programs that perform I/O use `main :: IO ()` and `do` blocks:

```haskell
main :: IO ()
main = do
  putStrLn "Hello!"
  putStrLn "What's your name?"
```

### Common I/O Functions

```haskell
putStrLn :: String -> IO ()    -- print with newline
putStr :: String -> IO ()      -- print without newline
print :: Show a => a -> IO ()  -- print any type
getLine :: IO String           -- read a line of input
```

### Simple Program Example

```haskell
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

### Reading Multiple Lines

```haskell
main :: IO ()
main = do
  putStr "Enter first number: "
  x <- readLine
  putStr "Enter second number: "
  y <- readLine
  putStrLn ("Sum: " ++ show (read x + read y))

readLine :: IO Int
readLine = read <$> getLine
```

---

## Practical Examples

### Example 1: Count Positive Numbers

**Recursive approach:**

```haskell
countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs) = (if x > 0 then 1 else 0) + countPositives xs
```

**Using list comprehension and length:**

```haskell
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]
```

### Example 2: Pythagorean Theorem

```haskell
square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth a b = square a + square b

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = pyth a b == square c
```

### Example 3: Capitalize String

```haskell
import Data.Char (toUpper, toLower)

capitalised :: String -> String
capitalised []     = []
capitalised (x:xs) = toUpper x : map toLower xs

-- Alternative using list comprehension
capitalised [] = []
capitalised (x:xs) = toUpper x : [toLower y | y <- xs]
```

### Example 4: Filter Evens and Halve

```haskell
halfEvens :: [Int] -> [Int]
halfEvens xs = [if even x then x `div` 2 else x | x <- xs]

-- Alternative
halfEvens xs = [x `div` 2 | x <- xs, even x]
```

### Example 5: In Range Filtering

```haskell
inRange :: Int -> Int -> [Int] -> [Int]
inRange low high xs = [x | x <- xs, x >= low, x <= high]

-- Example: inRange 5 10 [1..15] => [5,6,7,8,9,10]
```

### Example 6: Title Case

```haskell
import Data.Char (toUpper, toLower)

title :: [String] -> [String]
title words = [capitaliseWord w | w <- words]
  where
    capitaliseWord [] = []
    capitaliseWord (x:xs)
      | length (x:xs) >= 4 = toUpper x : map toLower xs
      | otherwise = map toLower (x:xs)

-- Example: title ["the", "bosun", "and", "the", "bridge"] 
-- => ["the", "Bosun", "and", "the", "Bridge"]
```

---

## Quick Reference Table

| Concept | Example | Purpose |
|---------|---------|---------|
| Value binding | `x = 42` | Define a constant |
| Type annotation | `f :: Int -> Int` | Declare function type |
| Function def | `f x = x + 1` | Define a function |
| Lambda | `\x -> x + 1` | Anonymous function |
| List creation | `[1,2,3]` | Create a list |
| Cons operator | `1 : [2,3]` | Prepend to list |
| List comprehension | `[x^2 | x <- [1..5]]` | Generate list |
| Pattern matching | `f (x:xs) = ...` | Unpack values |
| Guard | `f x \| x > 0 = ...` | Conditional logic |
| Where clause | `f x = a + b where a = ...` | Local definitions |
| Map | `map (*2) [1,2,3]` | Apply function to all |
| Filter | `filter even [1..10]` | Select elements |
| Fold | `foldr (+) 0 [1,2,3]` | Accumulate |
| If-then-else | `if cond then a else b` | Simple conditional |
| Case expression | `case x of pattern -> ...` | Pattern-based branch |
| String ops | `"hello" ++ " world"` | Concatenate strings |
| Module import | `import Data.Char` | Load module |
| I/O | `putStrLn "Hello"` | Input/output |

---

## Summary

This guide covers the essential theory of Haskell programming:

- **Pure functional paradigm:** Functions are first-class values, immutability is default
- **Strong typing:** Catch errors at compile time
- **Pattern matching & guards:** Powerful ways to express conditional logic
- **List operations:** Map, filter, fold, comprehensions
- **Recursion:** The replacement for loops
- **Higher-order functions:** Functions that work with functions

Master these concepts, and you'll be able to tackle most beginner to intermediate Haskell problems. Practice by writing functions, loading them in GHCi, and testing with examples.

---

**Last Updated:** November 2025
**Status:** Complete Theory Reference for Haskell Beginners