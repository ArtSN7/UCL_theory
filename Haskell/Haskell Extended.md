# Haskell: Complete Theory and Reference Guide (Extended Edition)

A comprehensive guide covering all essential Haskell concepts from fundamentals through advanced topics, organized by theme for easy reference. Includes extended sections on Higher-Order Functions and User-Defined Data Types.

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
18. [Strings & Characters](#strings--characters)
19. [Modules & Imports](#modules--imports)
20. [I/O Basics](#io-basics)
21. [Practical Examples](#practical-examples)
22. [Lab 3 Exercise Solutions](#lab-3-exercise-solutions)
23. [Quick Reference Table](#quick-reference-table)

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

### Polymorphic Types

Functions can work with multiple types using type variables:

```haskell
-- 'a' is a type variable - works with any type
length :: [a] -> Int
head :: [a] -> a
fst :: (a, b) -> a

-- Constrained polymorphism with type classes
max :: Ord a => a -> a -> a
sum :: Num a => [a] -> a
```

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

### The `$` Operator (Function Application)

The `$` operator applies a function to an argument with low precedence:

```haskell
-- These are equivalent:
f (g (h x))
f $ g $ h x

-- Useful to avoid parentheses:
sum (map (^2) [1..10])
sum $ map (^2) [1..10]
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
maximum [1,5,3]          -- 5
minimum [1,5,3]          -- 1
sum [1,2,3,4]            -- 10
product [1,2,3,4]        -- 24
and [True, True, False]  -- False
or [True, False, False]  -- True
all even [2,4,6]         -- True
any odd [2,4,6]          -- False
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
  | bmiVal < 18.5 = "Underweight"
  | bmiVal < 25.0 = "Normal"
  | bmiVal < 30.0 = "Overweight"
  | otherwise = "Obese"
  where
    bmiVal = weight / height ^ 2
```

---

## List Comprehensions

List comprehensions provide a declarative way to generate and filter lists.

### Basic Syntax

```haskell
[expression | generator, predicate]
```

### Simple Examples

```haskell
[x^2 | x <- [1..5]]           -- [1,4,9,16,25]
[x | x <- [1..10], even x]    -- [2,4,6,8,10]
[x^2 | x <- [1..10], x > 5]   -- [36,49,64,81,100]
```

### Multiple Generators

```haskell
[(x, y) | x <- [1,2], y <- ['a','b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

### Multiple Predicates

```haskell
[x | x <- [1..20], even x, x > 10]  -- [12,14,16,18,20]
```

---

## Higher-Order Functions (Extended)

Higher-order functions take functions as arguments or return functions. This is essential for Lab 3!

### Map

**Type:**
```haskell
map :: (a -> b) -> [a] -> [b]
```

**What it does:** Applies a function to every element of a list.

**Examples:**
```haskell
map (+1) [1,2,3]         -- [2,3,4]
map (^2) [1,2,3]         -- [1,4,9]
map toLower "HELLO"      -- "hello"
map show [1,2,3]         -- ["1","2","3"]
map (*2) [1,2,3,4]       -- [2,4,6,8]
```

**Implementation:**
```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

### Filter

**Type:**
```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

**What it does:** Keeps only elements that satisfy a predicate (return True).

**Examples:**
```haskell
filter even [1..10]       -- [2,4,6,8,10]
filter (>5) [1..10]       -- [6,7,8,9,10]
filter (>0) [-2,-1,0,1,2] -- [1,2]
filter (/= 'a') "banana"  -- "bnn"
```

**Implementation:**
```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

### Foldr (Fold Right)

**Type:**
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

**What it does:** Reduces a list to a single value by applying a function from right to left.

**Parameters:**
1. `(a -> b -> b)` - A combining function
2. `b` - An initial/base value (accumulator starting point)
3. `[a]` - The list to fold

**How it works:**
```
foldr f z [x1, x2, x3] = f x1 (f x2 (f x3 z))
```

**Examples:**
```haskell
foldr (+) 0 [1,2,3,4]     -- 10 (sum)
foldr (*) 1 [1,2,3,4]     -- 24 (product)
foldr (&&) True [True, True, False]  -- False
foldr (||) False [False, True, False] -- True
foldr (:) [] [1,2,3]      -- [1,2,3] (identity for lists)
foldr (++) "" ["a","b","c"] -- "abc"
foldr max 0 [1,5,3,2]     -- 5 (maximum, starting from 0)
```

**Visual Example for `foldr (+) 0 [1,2,3]`:**
```
foldr (+) 0 [1,2,3]
= 1 + (foldr (+) 0 [2,3])
= 1 + (2 + (foldr (+) 0 [3]))
= 1 + (2 + (3 + (foldr (+) 0 [])))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
```

**Implementation:**
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

### Foldl (Fold Left)

**Type:**
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
```

**What it does:** Reduces a list from left to right (tail-recursive, more efficient for large lists).

**How it works:**
```
foldl f z [x1, x2, x3] = f (f (f z x1) x2) x3
```

**Examples:**
```haskell
foldl (+) 0 [1,2,3,4]     -- 10
foldl (-) 0 [1,2,3]       -- -6 (0-1-2-3)
```

### Foldl1 and Foldr1 (No Initial Value)

Use the first element as the initial value:

```haskell
foldr1 :: (a -> a -> a) -> [a] -> a
foldl1 :: (a -> a -> a) -> [a] -> a

foldr1 max [1,5,3,2]  -- 5
foldr1 (+) [1,2,3]    -- 6
```

**Warning:** Crashes on empty list!

### Useful Patterns with Fold

**Sum:**
```haskell
sum' :: Num a => [a] -> a
sum' = foldr (+) 0
```

**Product:**
```haskell
product' :: Num a => [a] -> a
product' = foldr (*) 1
```

**And (all true):**
```haskell
and' :: [Bool] -> Bool
and' = foldr (&&) True
```

**Or (any true):**
```haskell
or' :: [Bool] -> Bool
or' = foldr (||) False
```

**Length:**
```haskell
length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0
```

**Reverse:**
```haskell
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
```

**Maximum:**
```haskell
maximum' :: Ord a => [a] -> a
maximum' = foldr1 max
```

### Defining Map with Foldr

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- Or equivalently:
myMap f = foldr ((:) . f) []
```

### Defining Filter with Foldr

```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []
```

### Custom Higher-Order Functions

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice (+1) 5  -- 7
applyTwice (*2) 3  -- 12
```

### Operator Sections

Partially apply operators:

```haskell
(+1)    -- adds 1 to its argument
(*2)    -- doubles its argument
(>5)    -- checks if greater than 5
(5>)    -- checks if 5 is greater than argument
(/2)    -- divides by 2
(2/)    -- divides 2 by argument
```

---

## User-Defined Data Types

### Type Declarations (Type Synonyms)

Create a new name for an existing type using `type`:

```haskell
type String = [Char]
```

`String` is now a synonym for `[Char]`.

**Making Types More Readable:**
```haskell
type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x - 1, y)
```

**Parameterized Type Synonyms:**
```haskell
type Pair a = (a, a)

mult :: Pair Int -> Int
mult (m, n) = m * n

copy :: a -> Pair a
copy x = (x, x)
```

### Data Declarations (New Types)

Create completely new types with `data`:

```haskell
data Bool = False | True
```

- `Bool` is a new type
- `False` and `True` are **constructors** (the only values of type `Bool`)

**Key Rules:**
- Type and constructor names must start with uppercase
- Values of new types can be used like built-in types

**Example: Custom Answer Type**
```haskell
data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown
```

### Constructors with Parameters

Constructors can take arguments:

```haskell
data Shape = Circle Float | Rect Float Float
```

- `Shape` has values like `Circle 5.0` or `Rect 3.0 4.0`
- Constructors are functions:
  - `Circle :: Float -> Shape`
  - `Rect :: Float -> Float -> Shape`

**Using Shapes:**
```haskell
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
```

### Parameterized Data Types

Data declarations can have type parameters:

```haskell
data Maybe a = Nothing | Just a
```

- `Maybe Int` can be `Nothing` or `Just 5`
- `Maybe String` can be `Nothing` or `Just "hello"`

**Safe Functions with Maybe:**
```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (m `div` n)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)
```

---

## Recursive Types

Types can be defined in terms of themselves.

### Natural Numbers

```haskell
data Nat = Zero | Succ Nat
```

- `Zero` represents 0
- `Succ` represents successor (n + 1)
- `Succ (Succ Zero)` represents 2

**Converting between Nat and Int:**
```haskell
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))
```

**Addition on Nat:**
```haskell
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
```

### Arithmetic Expressions

```haskell
data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
```

**Example:** `1 + (2 * 3)` is represented as:
```haskell
Add (Val 1) (Mul (Val 2) (Val 3))
```

**Evaluating Expressions:**
```haskell
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
```

**Counting Nodes:**
```haskell
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y
```

### Binary Trees

```haskell
data Tree a = Leaf a | Node (Tree a) a (Tree a)
```

**Example Tree:**
```haskell
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
```

**Tree Operations:**
```haskell
-- Check if element exists
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf v) = x == v
occurs x (Node left v right) = x == v || occurs x left || occurs x right

-- Flatten tree to list
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left x right) = flatten left ++ [x] ++ flatten right
```

---

## Strings & Characters

### What is a String?

A `String` is a list of characters: `String = [Char]`

```haskell
"hello" == ['h','e','l','l','o']  -- True
```

### Case Conversion

Import `Data.Char`:

```haskell
import Data.Char (toUpper, toLower)

toUpper 'a'          -- 'A'
toLower 'A'          -- 'a'
map toUpper "hello"  -- "HELLO"
```

---

## Modules & Imports

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

---

## I/O Basics

### The Do Notation

Programs that perform I/O use `main :: IO ()` and `do` blocks:

```haskell
main :: IO ()
main = do
  putStrLn "Hello!"
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

### Common I/O Functions

```haskell
putStrLn :: String -> IO ()    -- print with newline
putStr :: String -> IO ()      -- print without newline
print :: Show a => a -> IO ()  -- print any type
getLine :: IO String           -- read a line of input
```

---

## Practical Examples

### Example 1: Count Positive Numbers

```haskell
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

-- Using filter:
countPositives xs = length (filter (>0) xs)
```

### Example 2: Capitalize String

```haskell
import Data.Char (toUpper, toLower)

capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : map toLower xs
```

### Example 3: In Range Filtering

```haskell
inRange :: Int -> Int -> [Int] -> [Int]
inRange low high xs = filter (\x -> x >= low && x <= high) xs
```

---

## Lab 3 Exercise Solutions

Here are approaches for Lab Sheet 3 exercises using `map`, `filter`, and `foldr`:

### 1. mult - Product of a list
```haskell
mult :: Num a => [a] -> a
mult = foldr (*) 1

-- Example: mult [1,2,3,4] = 24
```

### 2. posList - Return only positive integers
```haskell
posList :: [Int] -> [Int]
posList = filter (>0)

-- Example: posList [-1,0,1,2,-3] = [1,2]
```

### 3. trueList - Check if all Booleans are True
```haskell
trueList :: [Bool] -> Bool
trueList = foldr (&&) True

-- Example: trueList [True, True, True] = True
-- Example: trueList [True, False, True] = False
```

### 4. evenList - Check if all numbers are even
```haskell
evenList :: [Int] -> Bool
evenList = foldr (\x acc -> even x && acc) True

-- Or using all:
evenList xs = foldr (&&) True (map even xs)

-- Example: evenList [2,4,6] = True
-- Example: evenList [2,3,4] = False
```

### 5. maxList - Maximum of a list (polymorphic)
```haskell
maxList :: Ord a => [a] -> a
maxList = foldr1 max

-- Example: maxList [3,1,4,1,5] = 5
```

### 6. inRange - Numbers within a range
```haskell
inRange :: Int -> Int -> [Int] -> [Int]
inRange low high = filter (\x -> x >= low && x <= high)

-- Example: inRange 2 5 [1,2,3,4,5,6] = [2,3,4,5]
```

### 7. countPositives - Count positive numbers
```haskell
countPositives :: [Int] -> Int
countPositives = length . filter (>0)

-- Or using foldr:
countPositives = foldr (\x acc -> if x > 0 then acc + 1 else acc) 0

-- Example: countPositives [-1,0,1,2,3] = 3
```

### 8. myLength - Using foldr and map
```haskell
myLength :: [a] -> Int
myLength xs = sum (map (\_ -> 1) xs)

-- Or:
myLength = sum . map (const 1)

-- Example: myLength [1,2,3,4,5] = 5
```

### 9. myMap - Define map using foldr
```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- Or:
myMap f = foldr ((:) . f) []

-- Example: myMap (*2) [1,2,3] = [2,4,6]
```

### 10. myLength' - Using foldr only
```haskell
myLength' :: [a] -> Int
myLength' = foldr (\_ acc -> acc + 1) 0

-- Example: myLength' [1,2,3,4,5] = 5
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
| List comprehension | `[x^2 \| x <- [1..5]]` | Generate list |
| Pattern matching | `f (x:xs) = ...` | Unpack values |
| Guard | `f x \| x > 0 = ...` | Conditional logic |
| Where clause | `f x = a + b where a = ...` | Local definitions |
| Map | `map (*2) [1,2,3]` | Apply function to all |
| Filter | `filter even [1..10]` | Select elements |
| Foldr | `foldr (+) 0 [1,2,3]` | Accumulate |
| Type synonym | `type Pos = (Int, Int)` | Alias for existing type |
| Data type | `data Bool = True \| False` | New type with constructors |
| Maybe | `Just 5` or `Nothing` | Optional value |
| Recursive type | `data Nat = Zero \| Succ Nat` | Self-referential type |

---

## Summary

This extended guide covers:

- **Core Haskell:** Types, functions, pattern matching, guards, recursion
- **Higher-Order Functions:** `map`, `filter`, `foldr`, `foldl` with detailed explanations
- **User-Defined Types:** `type` synonyms, `data` declarations, recursive types
- **Lab 3 Solutions:** All 10 exercises with idiomatic Haskell solutions

Master these concepts, and you'll be able to tackle most Haskell problems. Practice by writing functions, loading them in GHCi, and testing with examples.

---

**Last Updated:** November 2025  
**Status:** Extended Edition with Data Types and Lab 3 Solutions