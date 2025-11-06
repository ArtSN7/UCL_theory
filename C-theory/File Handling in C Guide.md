<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" style="height:64px;margin-right:32px"/>

# File Handling in C: Comprehensive Guide

This complete guide covers opening and closing files, reading and writing operations, file modes, file pointers, and comprehensive error checking.

***

## Part 1: File Fundamentals

### What is a File?

A **file** is a collection of data stored on disk. File handling allows you to:

- Read data from files
- Write data to files
- Modify existing data
- Create new files
- Delete files (using system functions)


### File Pointer (FILE*)

A **FILE pointer** is a variable that represents an open file. It keeps track of:

- The file's current position
- Whether it's open or closed
- Whether you're reading or writing
- Any errors that occurred

**Declaration:**

```c
FILE* fp;  // FILE* is a pointer to a FILE structure
```


***

## Part 2: Opening Files with fopen()

### Purpose

The `fopen()` function opens a file and returns a pointer to it.

### Syntax

```c
FILE* fopen(const char* filename, const char* mode);
```

**Parameters:**

- `filename`: String containing the file name or path
- `mode`: String specifying how to open the file

**Returns:**

- Pointer to FILE on success
- `NULL` if the file cannot be opened


### File Modes

| Mode | Purpose | File Behavior |
| :-- | :-- | :-- |
| `"r"` | **Read** | File must exist; opened for reading only |
| `"w"` | **Write** | Creates new file or truncates (empties) existing file |
| `"a"` | **Append** | Creates file if it doesn't exist; opens at end for writing |
| `"r+"` | **Read/Write** | File must exist; can read and write |
| `"w+"` | **Write/Read** | Creates or truncates file; can read and write |
| `"a+"` | **Append/Read** | Creates if needed; can read and append |
| `"rb"` | **Binary Read** | Opens file in binary mode for reading |
| `"wb"` | **Binary Write** | Opens file in binary mode for writing |
| `"ab"` | **Binary Append** | Opens file in binary mode for appending |
| `"r+b"` or `"rb+"` | **Binary Read/Write** | Binary mode, read and write |

**Text Mode vs Binary Mode:**

- **Text mode** (`"r"`, `"w"`, etc.): Handles line endings specially (platform-dependent)
- **Binary mode** (`"rb"`, `"wb"`, etc.): Reads/writes exact bytes without modification


### Examples

**Opening for Reading:**

```c
FILE* fp = fopen("input.txt", "r");
if (fp == NULL) {
    printf("Error: Could not open file for reading.\n");
    return 1;
}
// Perform read operations
fclose(fp);
```

**Opening for Writing (Creates or Overwrites):**

```c
FILE* fp = fopen("output.txt", "w");
if (fp == NULL) {
    printf("Error: Could not open file for writing.\n");
    return 1;
}
// Perform write operations
fclose(fp);
```

**Opening for Appending:**

```c
FILE* fp = fopen("log.txt", "a");
if (fp == NULL) {
    printf("Error: Could not open file for appending.\n");
    return 1;
}
// Append data to end of file
fclose(fp);
```

**Opening for Read and Write:**

```c
FILE* fp = fopen("data.txt", "r+");
if (fp == NULL) {
    printf("Error: Could not open file.\n");
    return 1;
}
// Can read and write
fclose(fp);
```


***

## Part 3: Closing Files with fclose()

### Purpose

The `fclose()` function closes an open file and flushes any buffered data to disk.

### Syntax

```c
int fclose(FILE* stream);
```

**Parameters:**

- `stream`: FILE pointer returned by `fopen()`

**Returns:**

- `0` on success
- `EOF` (usually -1) on error


### Important: Always Close Files

```c
FILE* fp = fopen("data.txt", "w");

// Write data...

fclose(fp);  // Close when done
fp = NULL;   // Good practice: set to NULL after closing
```

**Why close files?**

1. Flushes buffered data to disk (ensures all data is written)
2. Releases file system resources
3. Allows other programs to access the file
4. Prevents data corruption

### Error Checking

```c
FILE* fp = fopen("data.txt", "r");

if (fp == NULL) {
    perror("Error opening file");  // Print system error message
    return 1;
}

// File operations here...

if (fclose(fp) == EOF) {
    perror("Error closing file");
    return 1;
}
```


***

## Part 4: Reading Files

### 1. Character-Based Reading with fgetc()

**Purpose:** Read a single character from a file.

**Syntax:**

```c
int fgetc(FILE* stream);
```

**Returns:**

- Character as an `int` (ASCII value)
- `EOF` at end of file or error

**Example:**

```c
#include <stdio.h>

int main() {
    FILE* fp = fopen("data.txt", "r");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    int ch;
    while ((ch = fgetc(fp)) != EOF) {
        putchar(ch);  // Print each character
    }
    
    fclose(fp);
    return 0;
}
```


### 2. Line-Based Reading with fgets()

**Purpose:** Read a line of text from a file (safe string input).

**Syntax:**

```c
char* fgets(char* str, int n, FILE* stream);
```

**Parameters:**

- `str`: Buffer to store the line
- `n`: Maximum characters to read (including `\0`)
- `stream`: FILE pointer

**Returns:**

- Pointer to `str` on success
- `NULL` at EOF or error

**Important:** `fgets()` includes the newline character (`\n`) in the string.

**Example:**

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE* fp = fopen("input.txt", "r");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    char line[^100];
    while (fgets(line, sizeof(line), fp) != NULL) {
        printf("Read: %s", line);  // line already has \n
    }
    
    fclose(fp);
    return 0;
}
```

**Removing the Newline:**

```c
char line[^100];
fgets(line, sizeof(line), fp);

// Remove trailing newline
line[strcspn(line, "\n")] = '\0';
```


### 3. Formatted Reading with fscanf()

**Purpose:** Read formatted data from a file (like `scanf` but for files).

**Syntax:**

```c
int fscanf(FILE* stream, const char* format, ...);
```

**Returns:**

- Number of items successfully read
- `EOF` at end of file

**Example:**

```c
#include <stdio.h>

typedef struct {
    char name[^50];
    int age;
    float salary;
} Employee;

int main() {
    FILE* fp = fopen("employees.txt", "r");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    Employee emp;
    while (fscanf(fp, "%s %d %f", emp.name, &emp.age, &emp.salary) == 3) {
        printf("Name: %s, Age: %d, Salary: %.2f\n", 
               emp.name, emp.age, emp.salary);
    }
    
    fclose(fp);
    return 0;
}
```

**File format (employees.txt):**

```
Alice 30 50000
Bob 25 45000
Charlie 35 60000
```


### 4. Binary Reading with fread()

**Purpose:** Read binary data from a file (efficient for structures).

**Syntax:**

```c
size_t fread(void* ptr, size_t size, size_t count, FILE* stream);
```

**Parameters:**

- `ptr`: Buffer to store read data
- `size`: Size of each element
- `count`: Number of elements to read
- `stream`: FILE pointer

**Returns:**

- Number of items successfully read

**Example:**

```c
#include <stdio.h>
#include <string.h>

typedef struct {
    char name[^50];
    int id;
    float gpa;
} Student;

int main() {
    FILE* fp = fopen("students.dat", "rb");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    Student s;
    while (fread(&s, sizeof(Student), 1, fp) == 1) {
        printf("ID: %d, Name: %s, GPA: %.2f\n", s.id, s.name, s.gpa);
    }
    
    fclose(fp);
    return 0;
}
```


***

## Part 5: Writing Files

### 1. Character-Based Writing with fputc()

**Purpose:** Write a single character to a file.

**Syntax:**

```c
int fputc(int ch, FILE* stream);
```

**Returns:**

- Character written on success
- `EOF` on error

**Example:**

```c
FILE* fp = fopen("output.txt", "w");

if (fp == NULL) {
    perror("Error opening file");
    return 1;
}

fputc('H', fp);
fputc('i', fp);
fputc('\n', fp);

fclose(fp);
```


### 2. String-Based Writing with fputs()

**Purpose:** Write a string to a file.

**Syntax:**

```c
int fputs(const char* str, FILE* stream);
```

**Returns:**

- Non-negative number on success
- `EOF` on error

**Example:**

```c
FILE* fp = fopen("output.txt", "w");

if (fp == NULL) {
    perror("Error opening file");
    return 1;
}

fputs("Hello, World!\n", fp);
fputs("This is a file.\n", fp);

fclose(fp);
```


### 3. Formatted Writing with fprintf()

**Purpose:** Write formatted data to a file (like `printf` but for files).

**Syntax:**

```c
int fprintf(FILE* stream, const char* format, ...);
```

**Returns:**

- Number of characters written
- Negative value on error

**Example:**

```c
#include <stdio.h>

typedef struct {
    char name[^50];
    int id;
    float salary;
} Employee;

int main() {
    FILE* fp = fopen("employees.txt", "w");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    Employee emp1 = {"Alice", 101, 50000.0f};
    Employee emp2 = {"Bob", 102, 45000.0f};
    
    fprintf(fp, "Employee Database\n");
    fprintf(fp, "=================\n");
    fprintf(fp, "%s %d %.2f\n", emp1.name, emp1.id, emp1.salary);
    fprintf(fp, "%s %d %.2f\n", emp2.name, emp2.id, emp2.salary);
    
    if (fclose(fp) == EOF) {
        perror("Error closing file");
        return 1;
    }
    
    printf("Data written successfully!\n");
    return 0;
}
```

**Output file (employees.txt):**

```
Employee Database
=================
Alice 101 50000.00
Bob 102 45000.00
```


### 4. Binary Writing with fwrite()

**Purpose:** Write binary data to a file (efficient for structures).

**Syntax:**

```c
size_t fwrite(const void* ptr, size_t size, size_t count, FILE* stream);
```

**Parameters:**

- `ptr`: Data to write
- `size`: Size of each element
- `count`: Number of elements to write
- `stream`: FILE pointer

**Returns:**

- Number of items successfully written

**Example:**

```c
#include <stdio.h>
#include <string.h>

typedef struct {
    char name[^50];
    int id;
    float gpa;
} Student;

int main() {
    FILE* fp = fopen("students.dat", "wb");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    Student students[^3] = {
        {"Alice", 1, 3.8f},
        {"Bob", 2, 3.5f},
        {"Charlie", 3, 3.9f}
    };
    
    size_t written = fwrite(students, sizeof(Student), 3, fp);
    
    if (written != 3) {
        fprintf(stderr, "Error: Could not write all students.\n");
        fclose(fp);
        return 1;
    }
    
    printf("Successfully wrote %zu students.\n", written);
    fclose(fp);
    return 0;
}
```


***

## Part 6: File Pointer Management

### Moving the File Pointer with fseek()

**Purpose:** Move the file pointer to a specific position.

**Syntax:**

```c
int fseek(FILE* stream, long offset, int origin);
```

**Parameters:**

- `stream`: FILE pointer
- `offset`: Number of bytes to move
- `origin`: Starting position:
    - `SEEK_SET`: Beginning of file
    - `SEEK_CUR`: Current position
    - `SEEK_END`: End of file

**Returns:**

- `0` on success
- Non-zero on error

**Example:**

```c
FILE* fp = fopen("data.txt", "rb+");

if (fp == NULL) {
    perror("Error opening file");
    return 1;
}

// Move to 10th byte from start
fseek(fp, 10, SEEK_SET);

// Move 5 bytes forward from current position
fseek(fp, 5, SEEK_CUR);

// Move to 20 bytes before end
fseek(fp, -20, SEEK_END);

fclose(fp);
```


### Getting File Position with ftell()

**Purpose:** Get the current position of the file pointer.

**Syntax:**

```c
long ftell(FILE* stream);
```

**Returns:**

- Current file position (bytes from start)
- `-1L` on error

**Example:**

```c
FILE* fp = fopen("data.txt", "r");

long pos = ftell(fp);  // Current position (0 at start)
printf("Current position: %ld\n", pos);

fseek(fp, 10, SEEK_SET);
pos = ftell(fp);       // Position after seeking
printf("After seek: %ld\n", pos);

fclose(fp);
```


### Rewinding to Start with rewind()

**Purpose:** Move file pointer back to the beginning.

**Syntax:**

```c
void rewind(FILE* stream);
```

**Example:**

```c
FILE* fp = fopen("data.txt", "r");

// Read first line
char line[^100];
fgets(line, sizeof(line), fp);

// Go back to start
rewind(fp);

// Read first line again
fgets(line, sizeof(line), fp);

fclose(fp);
```


***

## Part 7: Comprehensive Error Checking

### Error Handling Functions

**feof() - Check End of File:**

```c
int feof(FILE* stream);
```

Returns non-zero if EOF has been reached.

**ferror() - Check for Errors:**

```c
int ferror(FILE* stream);
```

Returns non-zero if an error occurred.

**perror() - Print Error Message:**

```c
void perror(const char* message);
```

Prints system error message to stderr.

### Complete Error Handling Example

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE* fp = fopen("input.txt", "r");
    
    // Check if file opened successfully
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    char buffer[^100];
    int line_count = 0;
    
    // Read lines with error checking
    while (fgets(buffer, sizeof(buffer), fp) != NULL) {
        // Process line
        printf("Line %d: %s", ++line_count, buffer);
        
        // Check for read errors
        if (ferror(fp)) {
            perror("Error reading file");
            fclose(fp);
            return 1;
        }
    }
    
    // Check if EOF or error
    if (feof(fp)) {
        printf("End of file reached successfully.\n");
    } else if (ferror(fp)) {
        perror("Error occurred while reading");
        fclose(fp);
        return 1;
    }
    
    // Close file with error checking
    if (fclose(fp) == EOF) {
        perror("Error closing file");
        return 1;
    }
    
    printf("Total lines: %d\n", line_count);
    return 0;
}
```


***

## Part 8: Practical Examples

### Example 1: Copy File Contents

```c
#include <stdio.h>

int main() {
    FILE* source = fopen("source.txt", "r");
    FILE* dest = fopen("destination.txt", "w");
    
    if (source == NULL || dest == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    int ch;
    int count = 0;
    
    while ((ch = fgetc(source)) != EOF) {
        fputc(ch, dest);
        count++;
    }
    
    printf("Copied %d characters.\n", count);
    
    fclose(source);
    fclose(dest);
    
    return 0;
}
```


### Example 2: Count Lines in File

```c
#include <stdio.h>

int main() {
    FILE* fp = fopen("data.txt", "r");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    int ch;
    int lines = 0;
    
    while ((ch = fgetc(fp)) != EOF) {
        if (ch == '\n') {
            lines++;
        }
    }
    
    // If file doesn't end with newline, count last line
    if (ch != '\n' && ch != EOF) {
        lines++;
    }
    
    printf("Total lines: %d\n", lines);
    fclose(fp);
    
    return 0;
}
```


### Example 3: Search for Pattern in File

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE* fp = fopen("data.txt", "r");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    char line[^200];
    char pattern[] = "search_term";
    int found = 0;
    int line_num = 0;
    
    while (fgets(line, sizeof(line), fp) != NULL) {
        line_num++;
        if (strstr(line, pattern) != NULL) {
            printf("Line %d: %s", line_num, line);
            found++;
        }
    }
    
    printf("Found %d occurrences.\n", found);
    fclose(fp);
    
    return 0;
}
```


### Example 4: Write Structures to Binary File

```c
#include <stdio.h>
#include <string.h>

typedef struct {
    int id;
    char name[^50];
    int age;
    float salary;
} Employee;

int main() {
    FILE* fp = fopen("employees.bin", "wb");
    
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    Employee emp[^3] = {
        {1, "Alice", 30, 50000.0f},
        {2, "Bob", 28, 45000.0f},
        {3, "Charlie", 35, 60000.0f}
    };
    
    size_t written = fwrite(emp, sizeof(Employee), 3, fp);
    
    if (written == 3) {
        printf("Successfully wrote %zu employees.\n", written);
    } else {
        fprintf(stderr, "Error: Could not write all employees.\n");
    }
    
    fclose(fp);
    
    // Read back
    fp = fopen("employees.bin", "rb");
    
    if (fp == NULL) {
        perror("Error opening file for reading");
        return 1;
    }
    
    Employee read_emp;
    printf("\nReading employees:\n");
    while (fread(&read_emp, sizeof(Employee), 1, fp) == 1) {
        printf("ID: %d, Name: %s, Age: %d, Salary: %.2f\n",
               read_emp.id, read_emp.name, read_emp.age, read_emp.salary);
    }
    
    fclose(fp);
    return 0;
}
```


***

## Quick Reference: File Operations

| Function | Purpose | Returns |
| :-- | :-- | :-- |
| `fopen()` | Open file | FILE* or NULL |
| `fclose()` | Close file | 0 or EOF |
| `fgetc()` | Read character | Character or EOF |
| `fgets()` | Read line | Pointer or NULL |
| `fscanf()` | Read formatted | Items read |
| `fread()` | Read binary | Items read |
| `fputc()` | Write character | Character or EOF |
| `fputs()` | Write string | Non-negative or EOF |
| `fprintf()` | Write formatted | Characters written |
| `fwrite()` | Write binary | Items written |
| `fseek()` | Move pointer | 0 or non-zero |
| `ftell()` | Get position | Position or -1L |
| `rewind()` | Go to start | void |
| `feof()` | Check EOF | Non-zero or 0 |
| `ferror()` | Check error | Non-zero or 0 |
| `perror()` | Print error | void |


***

**Practice Challenges:**

1. Write a program that reads a text file and counts words, lines, and characters.
2. Create a program that reads employee data, modifies a record, and writes back to a file.
3. Build a simple text editor that reads, modifies, and saves files.
4. Write a program to copy a file and display progress.
5. Create a program that converts a text file to binary and back.

Which of these would you like to practice? Or should we move to the next topic (algorithms and sorting)?
<span style="display:none">[^1][^10][^2][^4][^5][^6][^7][^8][^9]</span>

<div align="center">⁂</div>

[^1]: https://www.w3resource.com/c-programming/c-file-handling.php

[^2]: https://www.oreilly.com/library/view/c-primer-plus/0672322226/0672322226_ch13lev1sec4.html

[^3]: https://www.geeksforgeeks.org/c/error-handling-during-file-operations-in-c-c/

[^4]: https://www.pudn.club/programming/c-file-io-tutorial-with-examples-fopen-fclose-fread-fwrite/

[^5]: https://www.codingeek.com/tutorials/c-programming/inputoutput-in-files/

[^6]: https://dev.to/thecodingcivilengineer/a-comprehensive-guide-to-file-manipulation-io-in-c-57ck

[^7]: https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm

[^8]: https://users.cs.utah.edu/~germain/PPS/Topics/C_Language/file_IO.html

[^9]: https://www.youtube.com/watch?v=U8JBoZmxH_o

[^10]: https://www.fresh2refresh.com/c-programming/c-file-handling/fopen-fclose-gets-fputs-functions-c/

