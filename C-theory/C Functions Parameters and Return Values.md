
# Functions in C: Parameters and Return Values

This comprehensive guide covers function parameters (pass by value and pass by reference/pointer), returning pointers, and working with arrays and strings in functions.

***

## Part 1: Function Basics Review

### Function Syntax

```c
return_type function_name(parameter_type1 param1, parameter_type2 param2) {
    // Function body
    return value;
}
```

**Example:**

```c
int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(5, 3);
    printf("%d\n", result); // Output: 8
    return 0;
}
```


***

## Part 2: Pass by Value

**Pass by value** means the function receives a **copy** of the argument's value, not the original variable itself.

### How Pass by Value Works

```c
void increment(int x) {
    x = x + 1;  // Modify the copy
    printf("Inside function: %d\n", x);
}

int main() {
    int num = 5;
    increment(num);           // Pass the value 5
    printf("After function: %d\n", num); // Still 5!
    return 0;
}
```

**Output:**

```
Inside function: 6
After function: 5
```

**Memory Layout:**

```
Original variable num:     Address 1000, Value 5

Function call increment(num):
- Copy of num created:     Address 2000, Value 5
- Copy is incremented:     Address 2000, Value 6
- Copy disappears when function ends

Back in main:
- Original num:            Address 1000, Value 5 (unchanged!)
```


### Key Points About Pass by Value

1. **Original variable is not modified** in the calling function
2. **Creates a copy** of the value, using extra memory
3. **Safe** - function can't accidentally damage original data
4. **Inefficient for large data** - copying large arrays/structs is slow

**Example with Different Types:**

```c
void modify(int x, float y, char z) {
    x = 100;
    y = 3.14f;
    z = 'Z';
    printf("Inside: x=%d, y=%.2f, z=%c\n", x, y, z);
}

int main() {
    int a = 10;
    float b = 1.5f;
    char c = 'A';
    
    modify(a, b, c);
    printf("Outside: a=%d, b=%.2f, c=%c\n", a, b, c);
    // Output: Outside: a=10, b=1.50, c=A (unchanged)
    return 0;
}
```


***

## Part 3: Pass by Reference (Using Pointers)

**Pass by reference** in C is simulated using **pointers**. The function receives the address of the original variable and can modify it directly.

### How Pass by Reference (Pointer) Works

```c
void increment(int* x) {           // Takes a pointer to int
    *x = *x + 1;                   // Dereference and modify
    printf("Inside function: %d\n", *x);
}

int main() {
    int num = 5;
    increment(&num);               // Pass the address
    printf("After function: %d\n", num); // Now 6!
    return 0;
}
```

**Output:**

```
Inside function: 6
After function: 6
```

**Memory Layout:**

```
Original variable num:     Address 1000, Value 5

Function call increment(&num):
- Pointer parameter x:     Address 2000, Value 1000 (address of num)
- Dereference *x (1000):   Access value at address 1000
- Modify *x to 6:          Address 1000 now contains 6
- Pointer disappears when function ends

Back in main:
- Original num:            Address 1000, Value 6 (CHANGED!)
```


### Key Points About Pass by Reference (Pointers)

1. **Original variable IS modified** in the calling function
2. **No copy** - only the address is passed (very efficient!)
3. **Powerful but risky** - function can modify caller's data
4. **Best for large data** - avoid copying large arrays/structs

### Practical Examples

**Example 1: Swap Two Variables**

```c
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int main() {
    int x = 10, y = 20;
    printf("Before: x=%d, y=%d\n", x, y);
    
    swap(&x, &y);
    
    printf("After: x=%d, y=%d\n", x, y);
    return 0;
}
```

**Output:**

```
Before: x=10, y=20
After: x=20, y=10
```

**Example 2: Multiple Returns via Pointers**

```c
void getMinMax(int arr[], int size, int* min, int* max) {
    *min = arr[^0];
    *max = arr[^0];
    
    for (int i = 1; i < size; i++) {
        if (arr[i] < *min) *min = arr[i];
        if (arr[i] > *max) *max = arr[i];
    }
}

int main() {
    int numbers[] = {5, 2, 9, 1, 7};
    int minimum, maximum;
    
    getMinMax(numbers, 5, &minimum, &maximum);
    
    printf("Min: %d, Max: %d\n", minimum, maximum);
    return 0;
}
```

**Output:**

```
Min: 1, Max: 9
```

**Example 3: Modifying Input Parameters**

```c
void uppercase(char* str) {
    while (*str != '\0') {
        if (*str >= 'a' && *str <= 'z') {
            *str = *str - 32;  // Convert to uppercase
        }
        str++;  // Move to next character
    }
}

int main() {
    char text[] = "hello";
    printf("Before: %s\n", text);
    
    uppercase(text);
    
    printf("After: %s\n", text);
    return 0;
}
```

**Output:**

```
Before: hello
After: HELLO
```


### Pass by Value vs Pass by Reference Comparison

| Aspect | Pass by Value | Pass by Reference (Pointer) |
| :-- | :-- | :-- |
| **Syntax** | `void func(int x)` | `void func(int* x)` |
| **Call** | `func(num)` | `func(&num)` |
| **Access** | Use `x` directly | Dereference with `*x` |
| **Original Modified** | No | Yes |
| **Memory** | Creates copy | No copy, just address |
| **Speed** | Slower for large data | Faster (no copying) |
| **Safety** | Safer (can't damage original) | Riskier (can modify original) |
| **Use Case** | Small values, no modification needed | Need to modify, or large data |


***

## Part 4: Returning Pointers from Functions

### Basic Pointer Returns

**Syntax:**

```c
type* function_name() {
    // ...
    return pointer;
}
```


### Returning Pointers to Static Variables

**Static variables** persist after function execution, making them safe to return.

```c
int* getStaticAddress() {
    static int value = 42;  // Static: survives function call
    return &value;
}

int main() {
    int* ptr = getStaticAddress();
    printf("%d\n", *ptr);  // Output: 42
    
    // Can call again and get same address
    int* ptr2 = getStaticAddress();
    printf("%d\n", *ptr2);  // Output: 42
    printf("%p\n", ptr);    // Same address!
    printf("%p\n", ptr2);   // Same address!
    
    return 0;
}
```


### Returning Pointers to Dynamically Allocated Memory

The most common use case - allocate memory, return pointer to it.

```c
int* createArray(int size) {
    int* arr = (int*)malloc(size * sizeof(int));
    return arr;  // Safe: heap memory persists
}

int main() {
    int* myArray = createArray(5);
    
    for (int i = 0; i < 5; i++) {
        myArray[i] = i * 10;
    }
    
    for (int i = 0; i < 5; i++) {
        printf("%d ", myArray[i]);
    }
    printf("\n");
    
    free(myArray);
    myArray = NULL;
    
    return 0;
}
```

**Output:**

```
0 10 20 30 40
```


### Dangerous: Returning Pointer to Local Variable (DON'T DO THIS!)

```c
int* dangerousFunction() {
    int x = 10;        // Local variable on stack
    return &x;         // DANGER! x disappears after function ends
}

int main() {
    int* ptr = dangerousFunction();
    printf("%d\n", *ptr);  // Undefined behavior! (might print garbage)
    return 0;
}
```

**Why it's dangerous:**

- `x` is created on the stack during function execution
- When the function returns, `x` is destroyed
- The pointer points to freed/reused memory
- Reading `*ptr` gives unpredictable results (garbage or crash)


### Safe Ways to Return Pointers

**Method 1: Return Dynamically Allocated Memory**

```c
char* getString() {
    char* str = (char*)malloc(50 * sizeof(char));
    strcpy(str, "Hello, World!");
    return str;  // Safe: heap memory persists
}

int main() {
    char* msg = getString();
    printf("%s\n", msg);
    free(msg);  // Must free!
    msg = NULL;
    return 0;
}
```

**Method 2: Return Pointer to Static Variable**

```c
char* getStaticString() {
    static char str[^50] = "Static String";
    return str;  // Safe: static persists
}

int main() {
    char* msg = getStaticString();
    printf("%s\n", msg);
    // Don't free static memory!
    return 0;
}
```

**Method 3: Accept Pointer as Parameter (Fill It)**

```c
void getString(char* str, int size) {
    snprintf(str, size, "Filled string");  // Fill caller's buffer
}

int main() {
    char buffer[^100];
    getString(buffer, sizeof(buffer));
    printf("%s\n", buffer);
    return 0;
}
```


***

## Part 5: Functions Operating on Arrays

### 1D Arrays as Function Parameters

**Important:** Arrays decay to pointers when passed to functions.

**Declaration Styles (All Equivalent):**

```c
void printArray(int arr[], int size) { ... }
void printArray(int* arr, int size) { ... }  // Same thing!
```

**Example:**

```c
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

int main() {
    int numbers[] = {10, 20, 30, 40, 50};
    printArray(numbers, 5);  // Pass array name (decays to pointer)
    return 0;
}
```

**Output:**

```
10 20 30 40 50
```

**Important:** `sizeof()` Inside Function Doesn't Work!

```c
void wrongSize(int arr[]) {
    printf("%lu\n", sizeof(arr));  // Size of pointer, not array!
    // Output: 8 (on 64-bit system), not the actual array size
}

int main() {
    int arr[^5] = {1, 2, 3, 4, 5};
    wrongSize(arr);
    
    printf("%lu\n", sizeof(arr));  // Works here: 20 (5 ints * 4 bytes)
    return 0;
}
```

**Solution: Always Pass Size**

```c
void processArray(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        arr[i] *= 2;  // Double each element
    }
}

int main() {
    int numbers[] = {1, 2, 3, 4, 5};
    processArray(numbers, 5);
    
    for (int i = 0; i < 5; i++) {
        printf("%d ", numbers[i]);
    }
    return 0;
}
```

**Output:**

```
2 4 6 8 10
```


### 2D Arrays as Function Parameters

**Syntax:** Must specify column size (row size can be variable, but usually specify it too)

```c
void print2D(int arr[][^3], int rows) {  // 3 is column size (required)
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < 3; j++) {
            printf("%d ", arr[i][j]);
        }
        printf("\n");
    }
}

int main() {
    int grid[^2][^3] = {
        {1, 2, 3},
        {4, 5, 6}
    };
    
    print2D(grid, 2);
    return 0;
}
```

**Output:**

```
1 2 3
4 5 6
```

**Pointer to 2D Array:**

```c
void print2D(int (*arr)[^3], int rows) {  // Pointer to array of 3 ints
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < 3; j++) {
            printf("%d ", arr[i][j]);
        }
        printf("\n");
    }
}
```


### Variable-Sized 2D Arrays (Ragged)

For truly dynamic 2D arrays with variable column sizes:

```c
void fillRagged(int** matrix, int rows, int cols[]) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols[i]; j++) {
            matrix[i][j] = i * 10 + j;
        }
    }
}

void printRagged(int** matrix, int rows, int cols[]) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols[i]; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
}

int main() {
    int rows = 3;
    int cols[] = {2, 3, 2};
    
    // Allocate ragged array
    int** matrix = (int**)malloc(rows * sizeof(int*));
    for (int i = 0; i < rows; i++) {
        matrix[i] = (int*)malloc(cols[i] * sizeof(int));
    }
    
    fillRagged(matrix, rows, cols);
    printRagged(matrix, rows, cols);
    
    // Free ragged array
    for (int i = 0; i < rows; i++) {
        free(matrix[i]);
    }
    free(matrix);
    
    return 0;
}
```

**Output:**

```
0 1
10 11 12
20 21
```


***

## Part 6: Functions Operating on Strings

### String Functions with Pointers

**Example 1: String Length Function**

```c
int stringLength(char* str) {
    int count = 0;
    while (*str != '\0') {
        count++;
        str++;  // Move to next character
    }
    return count;
}

int main() {
    char text[] = "Hello";
    printf("Length: %d\n", stringLength(text));
    return 0;
}
```

**Output:**

```
Length: 5
```

**Example 2: String Copy Function**

```c
void stringCopy(char* dest, char* src) {
    while (*src != '\0') {
        *dest = *src;
        dest++;
        src++;
    }
    *dest = '\0';  // Don't forget null terminator!
}

int main() {
    char source[] = "Copy Me";
    char destination[^50];
    
    stringCopy(destination, source);
    printf("%s\n", destination);
    return 0;
}
```

**Output:**

```
Copy Me
```

**Example 3: String Concatenate Function**

```c
void stringConcat(char* dest, char* src) {
    // Find end of dest
    while (*dest != '\0') {
        dest++;
    }
    
    // Copy src to end of dest
    while (*src != '\0') {
        *dest = *src;
        dest++;
        src++;
    }
    *dest = '\0';
}

int main() {
    char str1[^50] = "Hello ";
    char str2[] = "World";
    
    stringConcat(str1, str2);
    printf("%s\n", str1);
    return 0;
}
```

**Output:**

```
Hello World
```

**Example 4: Array of Strings**

```c
void printStrings(char* strings[], int count) {
    for (int i = 0; i < count; i++) {
        printf("%d: %s\n", i + 1, strings[i]);
    }
}

int main() {
    char* names[] = {
        "Alice",
        "Bob",
        "Charlie",
        "Diana"
    };
    
    printStrings(names, 4);
    return 0;
}
```

**Output:**

```
1: Alice
2: Bob
3: Charlie
4: Diana
```

**Example 5: Reverse a String**

```c
void reverseString(char* str) {
    char* start = str;
    char* end = str;
    
    // Find end
    while (*end != '\0') {
        end++;
    }
    end--;  // Point to last character (before \0)
    
    // Swap characters
    while (start < end) {
        char temp = *start;
        *start = *end;
        *end = temp;
        
        start++;
        end--;
    }
}

int main() {
    char text[] = "Hello";
    printf("Before: %s\n", text);
    
    reverseString(text);
    
    printf("After: %s\n", text);
    return 0;
}
```

**Output:**

```
Before: Hello
After: olleH
```


***

## Part 7: Function Parameters Comparison Table

| Scenario | Pass by Value | Pass by Reference |
| :-- | :-- | :-- |
| **Modify original variable** | No | Yes |
| **Small data (int, char)** | Use this | Use value (no need for pointer) |
| **Large data (array, struct)** | Inefficient | Use this |
| **Multiple returns** | Can't use | Use pointers |
| **Syntax** | `func(var)` | `func(&var)` |
| **Inside function** | Use normally | Dereference with `*` |
| **Danger** | Low | Can corrupt caller's data if not careful |


***

## Part 8: Complete Practical Example

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
int* createArray(int size);
void fillArray(int* arr, int size, int start_value);
void printArray(int* arr, int size);
void freeArray(int* arr);

// Function implementations
int* createArray(int size) {
    int* arr = (int*)malloc(size * sizeof(int));
    if (arr == NULL) {
        printf("Memory allocation failed!\n");
        return NULL;
    }
    return arr;
}

void fillArray(int* arr, int size, int start_value) {
    for (int i = 0; i < size; i++) {
        arr[i] = start_value + i;
    }
}

void printArray(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

void freeArray(int* arr) {
    free(arr);
    arr = NULL;  // Note: doesn't affect caller's pointer
}

int main() {
    int* myArray = createArray(5);
    
    if (myArray == NULL) {
        return 1;
    }
    
    fillArray(myArray, 5, 10);
    printf("Array: ");
    printArray(myArray, 5);
    
    freeArray(myArray);
    // myArray still points to freed memory (dangerous!)
    myArray = NULL;  // Should do this in caller
    
    return 0;
}
```

**Output:**

```
Array: 10 11 12 13 14
```


***

**Practice Challenges:**

1. Write a function that takes two integers by reference and swaps them.
2. Create a function that finds the maximum value in an array.
3. Write a function that modifies a string to uppercase (in-place).
4. Create a function that returns a dynamically allocated array and fills it with values.
5. Write a function that counts how many times a character appears in a string.

Which of these would you like to practice? Or should we move to the next topic (structures)?
<span style="display:none">[^1][^10][^4][^6][^7][^8][^9]</span>

<div align="center">⁂</div>

[^1]: https://www.w3schools.com/cpp/cpp_function_reference.asp

[^2]: https://www.tutorialspoint.com/cprogramming/c_return_pointer_from_functions.htm

[^3]: https://stackoverflow.com/questions/30999951/passing-array-of-strings-to-functions-c

[^4]: https://www.geeksforgeeks.org/cpp/pass-by-reference-in-c/

[^5]: https://www.geeksforgeeks.org/cpp/returning-a-function-pointer-from-a-function-in-c-cpp/

[^6]: https://www.tutorialspoint.com/cprogramming/c_passing_arrays_to_functions.htm

[^7]: https://dev.to/mikkel250/passing-by-value-passing-by-reference-in-c-1acg

[^8]: https://www.geeksforgeeks.org/c/how-to-return-a-pointer-from-a-function-in-c/

[^9]: https://www.programiz.com/c-programming/c-arrays-functions

[^10]: https://www.geeksforgeeks.org/c/difference-between-call-by-value-and-call-by-reference/

