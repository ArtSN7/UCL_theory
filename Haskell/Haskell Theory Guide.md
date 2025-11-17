<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" style="height:64px;margin-right:32px"/>

# Haskell Theory Guide: Step by Step

This comprehensive guide is structured so you can learn Haskell from the start, step by step. Each key topic is presented with theory, examples, and explanations, making it suitable as a reference and cheat sheet for your studies.

## 1. **Intro \& Philosophy**

- Haskell is a pure, functional programming language.
- Core features: immutability, pure functions, strong static typing, and lazy evaluation.


## 2. **Installation \& First Launch**

- Install [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/downloads/) and optionally a simple editor or IDE.
- Run GHCi in terminal: `ghci`.
- Save programs as `.hs` files and load them in GHCi using `:load filename.hs`.


## 3. **Basic Syntax and Values**

- Statements end without semicolons; indentation matters.
- Define values:

```haskell
answer = 42
name = "Alice"
```

- Use comments with `--` for a single line or `{- ... -}` for blocks.


## 4. **First Program: Hello World**

```haskell
main = putStrLn "Hello, World!"
```

Run with `runhaskell hello.hs` or load in GHCi and type `main`.

## 5. **Types and Type Inference**

- Haskell is statically typed. The compiler usually infers types, but you can (and should!) annotate them:

```haskell
answer :: Int
answer = 42
```


### Common Types

- `Int` – fixed-size integer
- `Integer` – arbitrary precision integer
- `Double` – floating point
- `Bool` – logical value (`True`/`False`)
- `Char` – single character
- `String` – list of characters (`[Char]`)

Check types in GHCi: `:t value`

## 6. **Functions**

Functions are central in Haskell—the main building block.

### Defining Functions

```haskell
square :: Int -> Int
square x = x * x
```

- The signature shows: `square` takes `Int`, returns `Int`.
- Parameters are listed _without_ parentheses or commas.


### Multi-Argument Functions

```haskell
sumSquares :: Int -> Int -> Int
sumSquares x y = x^2 + y^2
```

Functions are _curried_: every function technically takes one argument and returns a new function.

### Calling Functions

```haskell
square 5         -- returns 25
sumSquares 3 4   -- returns 25
```

No parentheses or commas between arguments.

## 7. **Expressions and Operators**

- Arithmetic: `+`, `-`, `*`, `/`, `^` (power)
- Comparison: `==`, `/=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `not`

```haskell
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0
```


## 8. **Lists and Tuples**

### Lists

```haskell
numbers = [1, 2, 3, 4]
letters = ['a', 'b', 'c']
emptyList = []
```

Use `:` (cons) to build lists:

```haskell
moreNumbers = 0 : numbers -- [0, 1, 2, 3, 4]
```

Basic list functions:

- `head xs` – first element
- `tail xs` – everything but first
- `length xs` – number of elements
- `null xs` – is the list empty?
- `reverse xs` – reverse order


### Tuples

Tuples hold mixed types, fixed size.

```haskell
pair = (3, "cat")
triple = (True, 2.1, 'z')
```


## 9. **List Comprehensions \& Higher Order Functions**

### List Comprehensions

Like Python:

```haskell
squares = [x^2 | x <- [1..10]]
evens = [x | x <- [1..20], even x]
```


### Applying Functions to Lists

- `map` applies a function to each element:

```haskell
doubled = map (*2) [1,2,3]  -- [2,4,6]
```

- `filter` selects items:

```haskell
odds = filter odd [1,2,3,4] -- [1,3]
```

- `foldr` and `foldl` combine elements:

```haskell
sumList = foldr (+) 0 [1,2,3,4]  -- 10
```


## 10. **Pattern Matching**

Allows branching based on value structure:

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
```

Another example—head of a list:

```haskell
myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "Empty list!"
```


## 11. **Guards and Where Clauses**

### Guards

```haskell
absVal :: Int -> Int
absVal x
  | x < 0 = -x
  | otherwise = x
```


### Where Clauses

Define variables used in computation:

```haskell
sumSquares x y = squareX + squareY
  where
    squareX = x^2
    squareY = y^2
```


## 12. **Recursion**

Loops are typically written recursively.

```haskell
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
```


## 13. **Higher-Order Functions \& Lambda Expressions**

Functions can take other functions as parameters:

```haskell
applyTwice f x = f (f x)
```


### Lambda Expressions

Anonymous functions:

```haskell
map (\x -> x^2) [1,2,3]
-- returns [1,4,9]
```


## 14. **Strings \& Characters**

Strings are lists of characters:

```haskell
msg = "Hello"
greeting = head msg     -- 'H'
letters = tail msg      -- "ello"
```

Characters are encased in single quotes: `'a'`

## 15. **Module System and Importing Functions**

Organize code in modules and reuse functions:

```haskell
import Data.List
import qualified Data.Map as Map
```

Export functions from your own module with an explicit `module` declaration.

## 16. **Managing Errors (Basics)**

Use `error` for crashes (discouraged except for prototypes).
Prefer handling Maybe/Either types for safe error handling.

## 17. **IO (Input/Output) Basics**

Main function for running Haskell code:

```haskell
main :: IO ()
main = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name)
```


## 18. **Bringing It All Together—Typical Exercise Patterns**

- Write your functions in `.hs` files
- Load them with `:load filename.hs` in GHCi
- Test in GHCi interactively
- Use list comprehensions, pattern matching, and recursion to solve logic tasks

***

# Example: Summing Squares

```haskell
pyth :: Int -> Int -> Int
pyth x y = x^2 + y^2
```


# Example: Filtering Even Squares from List

```haskell
evens = filter even (map (^2) [1..10])   -- [4,16,36,64,100]
```


***

## **Summary \& Next Steps**

This sheet covers Haskell's basics up through list processing, error handling, pattern matching, and IO. It's enough for most beginner labs and practice tasks. If you need deeper theory, modules, or want practice exercises/examples—just ask!

**What topic would you like more examples or expanded explanations on next? Or do you want to try a practice round based on these principles?**

If you tell me your specific course or area, I can tailor future lessons even closer to your goals.
<span style="display:none">[^1][^2][^3][^4][^5][^6][^7][^8][^9]</span>

<div align="center">⁂</div>

[^1]: https://anton-k.github.io/ru-haskell-book/files/ru-haskell-book.pdf

[^2]: https://habr.com/ru/articles/152889/

[^3]: https://www.ohaskell.guide/epub/ohaskell.epub

[^4]: https://docs.google.com/document/d/1hBro8n8r-CCefiWu9Oz2t34q12rJTleZ5FcmQWZVenI/edit?usp=sharing

[^5]: https://www.lovemedia.net/articles/haskell-basics/

[^6]: https://rsdn.org/article/haskell/haskell_part1.xml

[^7]: https://tproger.ru/translations/becoming-productive-in-haskell

[^8]: https://api.pageplace.de/preview/DT0400.9785898186227_A48649535/preview-9785898186227_A48649535.pdf

[^9]: http://www.haskell.ru/haskell.pdf

