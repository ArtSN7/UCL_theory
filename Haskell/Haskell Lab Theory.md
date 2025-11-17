# Haskell Theory Cheat Sheet: From Basics to Advanced Lab Topics

## 1. Haskell: The Basics

**Paradigm:** Purely functional, statically typed, lazy evaluation.
- *No mutable variables:* Data is immutable; you never reassign variables.
- *Pure functions:* No side effects; functions always return the same result given the same inputs.
- *Laziness:* Computations don't happen until needed; enables infinite data structures.

---

## 2. Syntax & Getting Started
- Source files have `.hs` extension.
- Use GHCi (the interactive prompt):
    - Start with `ghci`
    - Load file: `:load MyFile.hs` or `:l MyFile.hs`
    - Reload after changes: `:reload` or `:r`
    - Evaluate expressions directly in GHCi

---

## 3. Values and Types
- **Primitive types**: `Int`, `Integer`, `Float`, `Double`, `Bool`, `Char`, `String`
- **Type annotations** (recommended):
    ```haskell
    x :: Int
    x = 10
    ```
- Determine the type in GHCi: `:t x`

---

## 4. Functions
- **Definition:**
    ```haskell
    double x = x * 2
    add a b = a + b
    ```
- **Type signature:**
    ```haskell
    add :: Int -> Int -> Int
    ```
- **Application:**
    - Use spaces: `add 2 3` (not parentheses)
- **Currying:** All functions technically take one argument and return a new function.
- **Anonymous (lambda) functions:**
    ```haskell
    \x -> x + 1
    ```

---

## 5. Lists
- **Create:**
    ```haskell
    xs = [1, 2, 3, 4]
    ? head xs        -- 1
    ? tail xs        -- [2,3,4]
    ```
- **Ranges:** `[1..5]` gives `[1,2,3,4,5]`
- **List Comprehensions:**
    ```haskell
    [x*2 | x <- [1..5], x*2 > 5]   -- [6, 8, 10]
    ```
- **Basic functions:** `map`, `filter`, `foldr`, `zip`, `length`, `sum`

---

## 6. Tuples
- **Usage:**
    ```haskell
    pair = (1, "hello")
    triple = (1, False, 'c')
    fst pair   -- 1
    snd pair   -- "hello"
    ```

---

## 7. Pattern Matching
- Reaction to value 'shapes', often with lists/tuples/constructors:
    ```haskell
    factorial 0 = 1
    factorial n = n * factorial (n-1)

    sumList [] = 0
    sumList (x:xs) = x + sumList xs
    ```

---

## 8. Guards
- Provide conditional logic:
    ```haskell
    abs x | x < 0 = -x
          | otherwise = x
    ```

---

## 9. Recursion
- The go-to looping mechanism in Haskell, can process lists and numbers:
    ```haskell
    count [] = 0
    count (_:xs) = 1 + count xs
    ```

---

## 10. Higher-Order Functions
- Functions that take or return functions:
    ```haskell
    applyTwice f x = f (f x)
    map :: (a -> b) -> [a] -> [b]
    ```

---

## 11. Working with Strings
- Strings are just lists of `Char`: `[Char]`
    ```haskell
    length "hello"    -- 5
    "He" ++ "llo"     -- "Hello"
    ```

---

## 12. Modules and Imports
- Organize code; import with `import Data.List` (or others)
    ```haskell
    import Data.Char (toUpper, toLower)
    ```
    Use `Data.Char.toUpper` to capitalize letters.

---

## 13. IO Basics
- Entry point of program: `main :: IO ()`
    ```haskell
    main = do
      putStrLn "Enter your name:"
      name <- getLine
      putStrLn ("Hello, " ++ name)
    ```

---

## 14. Example: List Comprehension and Functions for Labs
```haskell
square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth a b = square a + square b

halfEvens :: [Int] -> [Int]
halfEvens xs = [if even x then x `div` 2 else x | x <- xs]

countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

capitalised :: String -> String
capitalised []     = []
capitalised (x:xs) = toUpper x : [toLower y | y <- xs]

-- and other patterns as needed
```

---

## 15. Summary Table of Common Haskell Syntax and Functions
| Purpose | Example |
|---------|---------|
| Define function | `f x = x + 1` |
| Type declaration | `f :: Int -> Int` |
| List comp | `[x*2 | x <- [1..5]]` |
| Map | `map (+1) [1,2,3]` |
| Filter | `filter even [1,2,3,4]` |
| Pattern match | `f (x:xs) = ...` |
| Guard | `f x | x > 0 = ...` |
| Import | `import Data.List` |
| Lambda | `(\x -> x*x) 5` |
| IO | `putStrLn "Hi"` |

---

This cheat sheet covers the core language you'll need for your labs and more advanced work.

If you'd like to add real-world tips, worked walkthroughs, or theory expansions on any section, just let me know!