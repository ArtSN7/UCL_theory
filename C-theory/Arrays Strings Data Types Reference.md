

# Complete Arrays, Strings, and Data Types Reference for C

This comprehensive guide combines array declarations, string handling, all relevant string functions, and a complete data types reference table.

***

## Part 1: Arrays in C

### Array Declarations and Initialization

**1D Array (One-Dimensional)**

```c
int arr1[10]; // Declares an array of 10 integers
               // Elements are: arr1[0], arr1[1], ..., arr1[9]
               // Values are uninitialized (garbage values)
```

**With Initialization:**

```c
int arr2[3] = {10, 20, 30}; // Array with 3 elements, initialized with values
                             // arr2[0] = 10, arr2[1] = 20, arr2[2] = 30
```

**Partial Initialization:**

```c
int arr3[5] = {1, 2}; // First two elements are 1 and 2, rest are 0
                      // arr3 = {1, 2, 0, 0, 0}
```

**Without Size (Compiler Infers):**

```c
int arr4[] = {5, 10, 15}; // Compiler determines size is 3
```


### Accessing and Modifying Elements

```c
int arr2[3] = {10, 20, 30};

printf("%d\n", arr2[0]); // Output: 10
printf("%d\n", arr2[1]); // Output: 20

arr2[0] = 5; // Modify first element
printf("%d\n", arr2[0]); // Output: 5

// Now arr2 = {5, 20, 30}
```

**Index Out of Bounds:**

```c
int arr[5] = {1, 2, 3, 4, 5};
arr[10] = 100; // Undefined behavior! Writes to memory you don't own
               // This can crash your program or cause data corruption
```


### 2D Arrays (Two-Dimensional)

**Basic Declaration:**

```c
int grid[3][5]; // 3 rows, 5 columns
                // Total of 3 × 5 = 15 elements
                // Accessed as: grid[row][column]
                // grid[0][0], grid[0][1], ..., grid[2][4]
```

**With Initialization:**

```c
int grid[3][5] = {
    {1, 2, 3, 4, 5},      // Row 0
    {6, 7, 8, 9, 10},     // Row 1
    {11, 12, 13, 14, 15}  // Row 2
};

// Accessing elements:
printf("%d\n", grid[0][0]); // Output: 1 (first row, first column)
printf("%d\n", grid[1][2]); // Output: 8 (second row, third column)
printf("%d\n", grid[2][4]); // Output: 15 (third row, fifth column)
```

**Partial Initialization:**

```c
int grid[3][5] = {{1, 2}, {3}}; // Only some elements initialized, rest are 0
```

**Accessing Elements in Loops:**

```c
int grid[3][5];

// Fill the grid
for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
        grid[i][j] = i * 5 + j;
    }
}

// Print the grid
for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
        printf("%d ", grid[i][j]);
    }
    printf("\n"); // New line after each row
}
```

**Output:**

```
0 1 2 3 4
5 6 7 8 9
10 11 12 13 14
```


### Arrays and Memory Layout

Arrays are stored **contiguously** in memory. For `int arr[10]`:

- If `arr` starts at memory address 1000
- `arr[0]` is at 1000
- `arr[1]` is at 1004 (assuming 4-byte integers)
- `arr[2]` is at 1008
- And so on...

This is why **array indexing** is just pointer arithmetic under the hood.

***

## Part 2: Strings in C

### What is a String in C?

C **does not have a built-in string type**. Instead, a string is an **array of characters (`char`) that ends with a null terminator (`\0`)**.

**The null terminator** (`\0`) is the special character that marks the end of the string. It has ASCII value 0.

### String Declaration and Initialization

**Declaration with Initialization:**

```c
char name[10] = "Alex"; // Creates an array of 10 characters
                        // Filled as: 'A', 'l', 'e', 'x', '\0', ?, ?, ?, ?, ?
                        // The ? are uninitialized memory (garbage)
```

**What Actually Happens:**

- The string `"Alex"` has 4 characters
- Plus 1 null terminator (`\0`)
- Total of 5 characters needed
- Array size is 10, so there's extra space

**Memory Layout:**

```
Index:  0    1    2    3    4    5    6    7    8    9
Value: 'A'  'l'  'e'  'x' '\0'  ?    ?    ?    ?    ?
```

**Declaration Without Size (Risky!):**

```c
char name[] = "Alex"; // Compiler creates array of size 5 (4 chars + '\0')
                      // Safer because no risk of undersizing
```

**Character Array vs String:**

```c
char arr[5] = {'A', 'l', 'e', 'x', '\0'}; // This is a string (ends with '\0')
char arr2[4] = {'A', 'l', 'e', 'x'};      // This is NOT a string! No '\0'
```

**Why the Null Terminator Matters:**
Functions like `printf`, `strlen`, `strcpy` rely on finding the `\0` to know where the string ends. If it's missing, the function will read beyond the array, causing undefined behavior.

### Important String Facts

1. **String Literals are Read-Only:**

```c
char *ptr = "Hello"; // Points to a string literal (read-only)
ptr[0] = 'J';       // Undefined behavior! Can't modify
```

2. **Arrays are Mutable:**

```c
char name[10] = "Hello";
name[0] = 'J';      // OK! Now name is "Jello"
```

3. **Buffer Overflow:**

```c
char buffer[5] = "Hi";
strcat(buffer, "There"); // DANGER! "HiThere" is 7 chars + '\0'
                         // Buffer only has 5 slots!
```


***

## Part 3: String Functions

### `strlen` – String Length

**Purpose:** Get the number of characters in a string (not including the null terminator).

**Signature:**

```c
size_t strlen(const char *str);
```

**Returns:** The number of characters before the `\0`.

**Examples:**

```c
char name[] = "Alice";
printf("%lu\n", strlen(name)); // Output: 5

char empty[] = "";
printf("%lu\n", strlen(empty)); // Output: 0

char msg[] = "Hello, World!";
printf("%lu\n", strlen(msg)); // Output: 13
```

**How It Works Internally:**

```c
// Roughly equivalent to:
int my_strlen(const char *str) {
    int count = 0;
    while (str[count] != '\0') {
        count++;
    }
    return count;
}
```

**Using `strlen` to Remove Newline:**

```c
char line[100];
fgets(line, sizeof(line), stdin);

// Remove the trailing newline
int len = strlen(line);
if (line[len - 1] == '\n') {
    line[len - 1] = '\0';
}
```


***

### `strcpy` – String Copy

**Purpose:** Copy the entire contents of one string to another.

**Signature:**

```c
char *strcpy(char *dest, const char *src);
```

**Returns:** A pointer to `dest`.

**How It Works:**

- Copies every character from `src` to `dest`, including the null terminator
- **Modifies** `dest`
- `dest` must be large enough to hold the entire string!

**Examples:**

```c
char source[] = "Hello";
char destination[20]; // Must be large enough

strcpy(destination, source);
printf("%s\n", destination); // Output: Hello
```

**Danger – Buffer Overflow:**

```c
char source[] = "This is a very long string that won't fit";
char dest[10];

strcpy(dest, source); // CRASH! source is larger than dest can hold
```

**Safe Alternative – `strncpy`:**

```c
char source[] = "Hello, World!";
char dest[8];

strncpy(dest, source, 7); // Copy at most 7 characters
dest[7] = '\0';           // Manually add null terminator

printf("%s\n", dest); // Output: Hello, 
```

**Manual String Copy (Without Using strcpy):**

```c
char source[] = "Copy me";
char dest[20];

int i = 0;
while (source[i] != '\0') {
    dest[i] = source[i];
    i++;
}
dest[i] = '\0'; // Don't forget the null terminator!

printf("%s\n", dest); // Output: Copy me
```


***

### `strcmp` – String Compare

**Purpose:** Compare two strings lexicographically (alphabetically).

**Signature:**

```c
int strcmp(const char *str1, const char *str2);
```

**Returns:**

- `0` if the strings are identical
- **Negative number** if `str1` comes before `str2` alphabetically
- **Positive number** if `str1` comes after `str2` alphabetically

**Examples:**

```c
char str1[] = "apple";
char str2[] = "apple";
char str3[] = "banana";
char str4[] = "apricot";

printf("%d\n", strcmp(str1, str2)); // Output: 0 (equal)
printf("%d\n", strcmp(str1, str3)); // Output: negative (apple < banana)
printf("%d\n", strcmp(str3, str1)); // Output: positive (banana > apple)
printf("%d\n", strcmp(str1, str4)); // Output: negative (apple < apricot)
```

**Using in Conditions:**

```c
char password[] = "secret123";
char input[50];

printf("Enter password: ");
fgets(input, sizeof(input), stdin);
input[strcspn(input, "\n")] = '\0'; // Remove newline

if (strcmp(input, password) == 0) {
    printf("Access granted!\n");
} else {
    printf("Access denied!\n");
}
```

**Case-Sensitive Comparison:**

```c
printf("%d\n", strcmp("Hello", "hello")); // NOT zero! 'H' != 'h'
```

**For Case-Insensitive Comparison, Use `strcasecmp`:**

```c
#include <strings.h>

printf("%d\n", strcasecmp("Hello", "hello")); // Output: 0 (equal, ignoring case)
```

**How `strcmp` Works Internally:**

```c
// Roughly equivalent to:
int my_strcmp(const char *str1, const char *str2) {
    while (*str1 != '\0' && *str1 == *str2) {
        str1++;
        str2++;
    }
    return *str1 - *str2; // Returns difference of ASCII values
}
```


***

## Part 4: Common Data Types in C

### Complete Data Types Reference Table

| Type | Size (Typical) | Range | Example | Use Case |
| :-- | :-- | :-- | :-- | :-- |
| **`char`** | 1 byte | -128 to 127 (signed) or 0-255 (unsigned) | `'A'`, `'5'`, `'\n'` | Single character, small integers |
| **`unsigned char`** | 1 byte | 0 to 255 | Byte values | Raw bytes, extended ASCII |
| **`short`** | 2 bytes | -32,768 to 32,767 | `1000` | Small integers, memory-constrained |
| **`unsigned short`** | 2 bytes | 0 to 65,535 | `5000` | Non-negative small numbers |
| **`int`** | 4 bytes | -2,147,483,648 to 2,147,483,647 | `42`, `-15` | General-purpose integers |
| **`unsigned int`** | 4 bytes | 0 to 4,294,967,295 | `1000000` | Non-negative integers, counts |
| **`long`** | 4 or 8 bytes | Very large range | `1000000000L` | Large integers |
| **`unsigned long`** | 4 or 8 bytes | Very large range (0 and up) | `5000000000UL` | Large positive numbers |
| **`long long`** | 8 bytes | -9.2 × 10¹⁸ to 9.2 × 10¹⁸ | `1000000000000LL` | Very large integers |
| **`unsigned long long`** | 8 bytes | 0 to 1.8 × 10¹⁹ | `10000000000000ULL` | Very large positive integers |
| **`float`** | 4 bytes | ~3.4 × 10⁻³⁸ to 3.4 × 10³⁸ (6-7 decimal digits) | `3.14f`, `2.5f` | Floating-point numbers, moderate precision |
| **`double`** | 8 bytes | ~1.7 × 10⁻³⁰⁸ to 1.7 × 10³⁰⁸ (15-17 decimal digits) | `3.14`, `2.5` | Floating-point numbers, higher precision |
| **`long double`** | 8, 12, or 16 bytes | Extended precision | `3.14159265358979323846L` | Very high precision calculations |
| **`void`** | N/A | No type | — | Function return type, generic pointers |
| **`_Bool` or `bool`** | 1 byte | 0 (false) or 1 (true) | `true`, `false` | Boolean values (requires `<stdbool.h>`) |

### Format Specifiers for printf/scanf

| Type | printf | scanf | Example |
| :-- | :-- | :-- | :-- |
| `char` | `%c` | `%c` | `printf("%c\n", 'A');` |
| `int` | `%d` or `%i` | `%d` or `%i` | `printf("%d\n", 42);` |
| `unsigned int` | `%u` | `%u` | `printf("%u\n", 100);` |
| `float` | `%f` | `%f` | `printf("%.2f\n", 3.14);` |
| `double` | `%lf` | `%lf` | `scanf("%lf", &x);` |
| `long` | `%ld` | `%ld` | `printf("%ld\n", 1000L);` |
| `long long` | `%lld` | `%lld` | `printf("%lld\n", 1000000000000LL);` |
| `short` | `%hd` | `%hd` | `printf("%hd\n", 50);` |
| `string` | `%s` | `%s` | `printf("%s\n", "Hello");` |
| `pointer` | `%p` | N/A | `printf("%p\n", &x);` |
| `hexadecimal` | `%x` | `%x` | `printf("%x\n", 255);` // Output: ff |
| `octal` | `%o` | `%o` | `printf("%o\n", 8);` // Output: 10 |

### Data Type Sizes (Platform-Dependent)

To find the exact size on your system:

```c
#include <stdio.h>

int main() {
    printf("sizeof(char): %lu\n", sizeof(char));
    printf("sizeof(int): %lu\n", sizeof(int));
    printf("sizeof(float): %lu\n", sizeof(float));
    printf("sizeof(double): %lu\n", sizeof(double));
    printf("sizeof(long): %lu\n", sizeof(long));
    printf("sizeof(long long): %lu\n", sizeof(long long));
    
    return 0;
}
```

**Typical Output on 64-bit System:**

```
sizeof(char): 1
sizeof(int): 4
sizeof(float): 4
sizeof(double): 8
sizeof(long): 8
sizeof(long long): 8
```


***

## Part 5: Practical Examples Combining Everything

### Example 1: Array Manipulation

```c
#include <stdio.h>
#include <string.h>

int main() {
    int arr[5] = {1, 2, 3, 4, 5};
    
    printf("Array: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    // Modify element
    arr[2] = 100;
    
    printf("After modification: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    return 0;
}
```


### Example 2: 2D Array and Grid Processing

```c
#include <stdio.h>

int main() {
    int grid[3][4] = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    };
    
    printf("Grid contents:\n");
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 4; j++) {
            printf("%3d ", grid[i][j]); // Right-aligned in 3 spaces
        }
        printf("\n");
    }
    
    printf("\nElement at [1][2]: %d\n", grid[1][2]); // Output: 7
    
    return 0;
}
```


### Example 3: String Operations

```c
#include <stdio.h>
#include <string.h>

int main() {
    char name[50];
    char greeting[100];
    
    printf("Enter your name: ");
    fgets(name, sizeof(name), stdin);
    name[strcspn(name, "\n")] = '\0'; // Remove newline
    
    printf("Name length: %lu\n", strlen(name));
    
    // Copy and modify
    strcpy(greeting, "Hello, ");
    strcat(greeting, name);
    printf("%s\n", greeting);
    
    // Compare strings
    if (strcmp(name, "Alice") == 0) {
        printf("Welcome, Alice!\n");
    } else {
        printf("You are not Alice.\n");
    }
    
    return 0;
}
```


### Example 4: String Array (Array of Strings)

```c
#include <stdio.h>
#include <string.h>

int main() {
    char names[3][20] = {
        "Alice",
        "Bob",
        "Charlie"
    };
    
    printf("Names:\n");
    for (int i = 0; i < 3; i++) {
        printf("%d: %s (length: %lu)\n", i + 1, names[i], strlen(names[i]));
    }
    
    return 0;
}
```

**Output:**

```
Names:
1: Alice (length: 5)
2: Bob (length: 3)
3: Charlie (length: 7)
```


***

## Quick Reference: Key Points to Remember

| Concept | Key Point |
| :-- | :-- |
| **Arrays** | Indexed from 0; contiguous memory; fixed size at compile time |
| **2D Arrays** | `array[row][col]` layout; rows stored sequentially |
| **Strings** | Character arrays ending with `\0`; functions search for `\0` |
| **Array Bounds** | Accessing beyond bounds causes undefined behavior |
| **String Size** | Must account for `\0` in buffer sizing |
| **Buffer Overflow** | Occurs when data written exceeds allocated size |
| **VLAs** | Runtime-sized arrays; stack-allocated; C99 feature |
| **Data Types** | Choose appropriate size for your data range |
| **Null Terminator** | Essential for all string operations |


***

**Practice Challenges:**

1. Create a 5×5 matrix and fill it with numbers 1-25.
2. Write code to find the longest string in an array of strings.
3. Safely copy a user-input string and print its length.
4. Create a program that compares two strings and reports which comes first alphabetically.
5. Implement a function that removes all vowels from a string.

Which of these would you like to work through together?

