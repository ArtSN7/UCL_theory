

# Structures and Unions in C: Comprehensive Guide

This guide covers defining and using structs, arrays of structs, struct members with pointers, using structs for metadata, and introduces unions.

***

## Part 1: Understanding Structures

### What is a Structure?

A **structure** (or struct) is a user-defined data type that groups multiple variables (called **members**) of different data types into a single unit. It allows you to create complex data types tailored to your needs.

**Real-World Analogy:** A structure is like a template or form. Just as a form has multiple fields (name, address, phone), a struct has multiple members of different types.

### Structure Definition

**Basic Syntax:**

```c
struct structName {
    dataType member1;
    dataType member2;
    dataType member3;
};
```

**Example: Person Structure**

```c
struct Person {
    char name[^50];
    int age;
    float height;
    char gender;
};
```

**Important:** The definition ends with a **semicolon**.

***

## Part 2: Creating and Using Structure Variables

### Declaring Structure Variables

**Method 1: Separate Declaration**

```c
struct Person {
    char name[^50];
    int age;
    float height;
};

int main() {
    struct Person person1;  // Declare a variable of type struct Person
    return 0;
}
```

**Method 2: Declare at Definition (Optional)**

```c
struct Person {
    char name[^50];
    int age;
    float height;
} person1, person2;  // Create variables while defining struct
```

**Method 3: Using `typedef` (Recommended)**

```c
typedef struct {
    char name[^50];
    int age;
    float height;
} Person;

int main() {
    Person person1;  // No need for "struct" keyword
    return 0;
}
```

Or with struct name:

```c
typedef struct Person {
    char name[^50];
    int age;
    float height;
} Person;

int main() {
    Person p1;  // Can use without "struct"
    struct Person p2;  // Also works
    return 0;
}
```


### Accessing Structure Members

**Dot Operator (`.`):** For regular struct variables

```c
struct Person {
    char name[^50];
    int age;
    float height;
};

int main() {
    struct Person p1;
    
    // Assign values using dot operator
    strcpy(p1.name, "Alice");
    p1.age = 25;
    p1.height = 5.7f;
    
    // Access and print
    printf("Name: %s\n", p1.name);
    printf("Age: %d\n", p1.age);
    printf("Height: %.1f\n", p1.height);
    
    return 0;
}
```

**Arrow Operator (`->`):** For pointers to struct

```c
struct Person {
    char name[^50];
    int age;
};

int main() {
    struct Person p1;
    struct Person* ptr = &p1;  // Pointer to struct
    
    // Access using arrow operator
    strcpy(ptr->name, "Bob");
    ptr->age = 30;
    
    printf("Name: %s\n", ptr->name);
    printf("Age: %d\n", ptr->age);
    
    return 0;
}
```


### Initializing Structures

**At Declaration:**

```c
struct Person {
    char name[^50];
    int age;
    float height;
};

struct Person p1 = {"Alice", 25, 5.7f};  // Order matters
```

**Partial Initialization:**

```c
struct Person p1 = {"Alice", 25};  // height is uninitialized
struct Person p2 = {"Bob"};         // age and height are uninitialized
```

**Zero Initialization:**

```c
struct Person p1 = {0};  // All members set to 0 or '\0'
struct Person p2 = {};   // All members set to 0 or '\0'
```

**Named Initialization (C99+):**

```c
struct Person p1 = {
    .name = "Alice",
    .age = 25,
    .height = 5.7f
};
```


***

## Part 3: Memory Layout of Structures

### Structure Size

The size of a struct is the **sum of all member sizes**, potentially with **padding** for alignment.

```c
struct Simple {
    char c;      // 1 byte
    int i;       // 4 bytes
    float f;     // 4 bytes
};

int main() {
    printf("Size of struct: %lu bytes\n", sizeof(struct Simple));
    // Output: 12 bytes (with padding)
    
    // Without padding, it would be: 1 + 4 + 4 = 9 bytes
    // But due to alignment, it's 12 bytes
    return 0;
}
```

**Memory Alignment Example:**

```
Memory addresses:
0     1     2     3     4     5     6     7     8     9     10    11
[c]  [PAD]  [PAD]  [PAD]  [i - 4 bytes -]   [f - 4 bytes -]
```

The compiler adds padding to align members on boundaries for faster access.

### Checking Member Offsets

```c
struct Person {
    char name[^50];
    int age;
    float height;
};

int main() {
    printf("Offset of name: %lu\n", offsetof(struct Person, name));
    printf("Offset of age: %lu\n", offsetof(struct Person, age));
    printf("Offset of height: %lu\n", offsetof(struct Person, height));
    
    return 0;
}
```


***

## Part 4: Arrays of Structures

### Creating Array of Structs

```c
struct Student {
    int id;
    char name[^50];
    float gpa;
};

int main() {
    // Array of 3 students
    struct Student students[^3];
    
    // Assign values
    students[^0].id = 1;
    strcpy(students[^0].name, "Alice");
    students[^0].gpa = 3.8f;
    
    students[^1].id = 2;
    strcpy(students[^1].name, "Bob");
    students[^1].gpa = 3.5f;
    
    students[^2].id = 3;
    strcpy(students[^2].name, "Charlie");
    students[^2].gpa = 3.9f;
    
    // Access and print
    for (int i = 0; i < 3; i++) {
        printf("ID: %d, Name: %s, GPA: %.1f\n", 
               students[i].id, students[i].name, students[i].gpa);
    }
    
    return 0;
}
```

**Output:**

```
ID: 1, Name: Alice, GPA: 3.8
ID: 2, Name: Bob, GPA: 3.5
ID: 3, Name: Charlie, GPA: 3.9
```


### Initialize Array of Structs

```c
struct Point {
    int x;
    int y;
};

int main() {
    struct Point points[^3] = {
        {0, 0},
        {1, 2},
        {3, 4}
    };
    
    for (int i = 0; i < 3; i++) {
        printf("Point %d: (%d, %d)\n", i + 1, points[i].x, points[i].y);
    }
    
    return 0;
}
```

**Output:**

```
Point 1: (0, 0)
Point 2: (1, 2)
Point 3: (3, 4)
```


***

## Part 5: Structures with Pointer Members

### Why Use Pointers in Structures?

Pointers allow structures to:

1. Store dynamically allocated memory (strings, arrays)
2. Reference other structures (linked lists, trees)
3. Save memory (pointer is small, can reference large data)
4. Create self-referential structures

### Example 1: String Pointer Member

```c
struct Person {
    char* name;     // Pointer to dynamically allocated string
    int age;
    float salary;
};

int main() {
    struct Person p1;
    
    // Allocate memory for name
    p1.name = (char*)malloc(50 * sizeof(char));
    
    if (p1.name == NULL) {
        printf("Memory allocation failed!\n");
        return 1;
    }
    
    // Fill in data
    strcpy(p1.name, "Alice Johnson");
    p1.age = 30;
    p1.salary = 50000.0f;
    
    // Print
    printf("Name: %s, Age: %d, Salary: %.2f\n", p1.name, p1.age, p1.salary);
    
    // Free memory
    free(p1.name);
    p1.name = NULL;
    
    return 0;
}
```

**Output:**

```
Name: Alice Johnson, Age: 30, Salary: 50000.00
```


### Example 2: Array Pointer Member

```c
typedef struct {
    int* data;
    int size;
} Array;

int main() {
    Array arr;
    arr.size = 5;
    arr.data = (int*)malloc(5 * sizeof(int));
    
    if (arr.data == NULL) {
        printf("Memory allocation failed!\n");
        return 1;
    }
    
    // Fill array
    for (int i = 0; i < arr.size; i++) {
        arr.data[i] = i * 10;
    }
    
    // Print
    for (int i = 0; i < arr.size; i++) {
        printf("%d ", arr.data[i]);
    }
    printf("\n");
    
    // Free memory
    free(arr.data);
    arr.data = NULL;
    
    return 0;
}
```

**Output:**

```
0 10 20 30 40
```


### Example 3: Self-Referential Structure (Linked List Node)

```c
typedef struct Node {
    int data;
    struct Node* next;  // Pointer to next node
} Node;

int main() {
    // Create nodes
    Node* head = (Node*)malloc(sizeof(Node));
    Node* second = (Node*)malloc(sizeof(Node));
    Node* third = (Node*)malloc(sizeof(Node));
    
    // Assign data
    head->data = 10;
    second->data = 20;
    third->data = 30;
    
    // Link nodes
    head->next = second;
    second->next = third;
    third->next = NULL;
    
    // Print linked list
    Node* current = head;
    while (current != NULL) {
        printf("%d -> ", current->data);
        current = current->next;
    }
    printf("NULL\n");
    
    // Free memory
    free(head);
    free(second);
    free(third);
    
    return 0;
}
```

**Output:**

```
10 -> 20 -> 30 -> NULL
```


***

## Part 6: Nested Structures

### Embedding One Structure Inside Another

```c
typedef struct {
    int day;
    int month;
    int year;
} Date;

typedef struct {
    char name[^50];
    Date birthdate;  // Nested struct
} Person;

int main() {
    Person p1;
    
    strcpy(p1.name, "Alice");
    p1.birthdate.day = 15;
    p1.birthdate.month = 3;
    p1.birthdate.year = 1990;
    
    printf("Name: %s\n", p1.name);
    printf("Birthdate: %d/%d/%d\n", p1.birthdate.day, 
           p1.birthdate.month, p1.birthdate.year);
    
    return 0;
}
```

**Output:**

```
Name: Alice
Birthdate: 15/3/1990
```


### Pointer to Nested Structure

```c
typedef struct {
    int day;
    int month;
    int year;
} Date;

typedef struct {
    char* name;
    Date* birthdate;  // Pointer to Date struct
} Person;

int main() {
    Person p1;
    
    // Allocate memory
    p1.name = (char*)malloc(50 * sizeof(char));
    p1.birthdate = (Date*)malloc(sizeof(Date));
    
    strcpy(p1.name, "Bob");
    p1.birthdate->day = 20;
    p1.birthdate->month = 7;
    p1.birthdate->year = 1985;
    
    printf("Name: %s\n", p1.name);
    printf("Birthdate: %d/%d/%d\n", p1.birthdate->day,
           p1.birthdate->month, p1.birthdate->year);
    
    // Free memory
    free(p1.name);
    free(p1.birthdate);
    
    return 0;
}
```


***

## Part 7: Structures for Metadata (Ragged 2D Array Example)

A common use case is storing metadata about data structures.

### Ragged Array with Metadata

```c
typedef struct {
    int** data;      // Pointer to ragged array
    int rows;        // Number of rows
    int* cols;       // Pointer to array of column sizes
} RaggedArray;

// Create a ragged array
RaggedArray* createRaggedArray(int rows, int col_sizes[]) {
    RaggedArray* arr = (RaggedArray*)malloc(sizeof(RaggedArray));
    
    arr->rows = rows;
    arr->cols = (int*)malloc(rows * sizeof(int));
    arr->data = (int**)malloc(rows * sizeof(int*));
    
    // Copy column sizes and allocate each row
    for (int i = 0; i < rows; i++) {
        arr->cols[i] = col_sizes[i];
        arr->data[i] = (int*)malloc(col_sizes[i] * sizeof(int));
    }
    
    return arr;
}

// Free a ragged array
void freeRaggedArray(RaggedArray* arr) {
    for (int i = 0; i < arr->rows; i++) {
        free(arr->data[i]);
    }
    free(arr->data);
    free(arr->cols);
    free(arr);
}

// Print ragged array
void printRaggedArray(RaggedArray* arr) {
    for (int i = 0; i < arr->rows; i++) {
        for (int j = 0; j < arr->cols[i]; j++) {
            printf("%d ", arr->data[i][j]);
        }
        printf("\n");
    }
}

int main() {
    int col_sizes[] = {3, 4, 2};
    RaggedArray* arr = createRaggedArray(3, col_sizes);
    
    // Fill array
    int value = 1;
    for (int i = 0; i < arr->rows; i++) {
        for (int j = 0; j < arr->cols[i]; j++) {
            arr->data[i][j] = value++;
        }
    }
    
    // Print
    printRaggedArray(arr);
    
    // Free
    freeRaggedArray(arr);
    
    return 0;
}
```

**Output:**

```
1 2 3
4 5 6 7
8 9
```


***

## Part 8: Functions with Structures

### Pass Structure by Value

```c
struct Point {
    int x;
    int y;
};

void printPoint(struct Point p) {  // Pass by value
    printf("Point: (%d, %d)\n", p.x, p.y);
}

void movePoint(struct Point p) {   // Changes don't affect original
    p.x += 10;
    p.y += 10;
    printf("Inside function: (%d, %d)\n", p.x, p.y);
}

int main() {
    struct Point p = {5, 5};
    
    printPoint(p);
    movePoint(p);
    printPoint(p);  // Still {5, 5}
    
    return 0;
}
```

**Output:**

```
Point: (5, 5)
Inside function: (15, 15)
Point: (5, 5)
```


### Pass Structure by Reference (Using Pointer)

```c
struct Person {
    char name[^50];
    int age;
};

void updateAge(struct Person* p, int newAge) {  // Pass by reference
    p->age = newAge;  // Use arrow operator
}

int main() {
    struct Person p = {"Alice", 25};
    printf("Before: Age = %d\n", p.age);
    
    updateAge(&p, 30);
    printf("After: Age = %d\n", p.age);
    
    return 0;
}
```

**Output:**

```
Before: Age = 25
After: Age = 30
```


### Return Structure from Function

```c
struct Point {
    int x;
    int y;
};

struct Point createPoint(int x, int y) {
    struct Point p = {x, y};
    return p;  // Return by value
}

int main() {
    struct Point p = createPoint(3, 4);
    printf("Point: (%d, %d)\n", p.x, p.y);
    
    return 0;
}
```


### Return Pointer to Dynamically Allocated Structure

```c
typedef struct {
    char name[^50];
    int id;
} Student;

Student* createStudent(char name[], int id) {
    Student* s = (Student*)malloc(sizeof(Student));
    strcpy(s->name, name);
    s->id = id;
    return s;
}

int main() {
    Student* s = createStudent("Alice", 101);
    printf("ID: %d, Name: %s\n", s->id, s->name);
    free(s);
    return 0;
}
```


***

## Part 9: Unions in C

### What is a Union?

A **union** is similar to a structure, but **all members share the same memory space**. Only one member can hold a value at any given time. The size of a union is the size of its largest member.

**Syntax:**

```c
union unionName {
    dataType member1;
    dataType member2;
    dataType member3;
};
```


### Structure vs Union Comparison

| Aspect | Structure | Union |
| :-- | :-- | :-- |
| **Memory** | Each member has separate memory | All members share same memory |
| **Size** | Sum of all member sizes | Size of largest member |
| **Values** | All members can hold values simultaneously | Only one member can hold value at a time |
| **Data Integrity** | All values preserved | Only last assigned value retained |
| **Usage** | Store multiple related data | Memory-efficient alternatives, type punning |

### Union Example

```c
union Data {
    int i;
    float f;
    char c;
};

int main() {
    union Data data;
    
    printf("Size of union: %lu bytes\n", sizeof(union Data));
    // Output: 4 bytes (size of largest member, float or int)
    
    data.i = 10;
    printf("data.i: %d\n", data.i);
    printf("data.f: %f\n", data.f);  // Garbage value
    
    data.f = 3.14f;
    printf("\nAfter setting f to 3.14:\n");
    printf("data.i: %d\n", data.i);  // Changed!
    printf("data.f: %f\n", data.f);
    
    data.c = 'A';
    printf("\nAfter setting c to 'A':\n");
    printf("data.i: %d\n", data.i);  // Changed again!
    printf("data.f: %f\n", data.f);
    printf("data.c: %c\n", data.c);
    
    return 0;
}
```

**Output:**

```
Size of union: 4 bytes
data.i: 10
data.f: 0.000000

After setting f to 3.14:
data.i: 1078205669
data.f: 3.140000

After setting c to 'A':
data.i: 65
data.f: 0.000000
data.c: A
```

**Explanation:** Each assignment overwrites the shared memory, affecting other members.

### Union Use Case: Hardware Register

```c
typedef union {
    unsigned int full;  // 32-bit register
    struct {
        unsigned char byte0;
        unsigned char byte1;
        unsigned char byte2;
        unsigned char byte3;
    } bytes;
} Register;

int main() {
    Register reg;
    
    reg.full = 0xAABBCCDD;
    
    printf("Full: 0x%X\n", reg.full);
    printf("Byte 0: 0x%X\n", reg.bytes.byte0);
    printf("Byte 1: 0x%X\n", reg.bytes.byte1);
    printf("Byte 2: 0x%X\n", reg.bytes.byte2);
    printf("Byte 3: 0x%X\n", reg.bytes.byte3);
    
    return 0;
}
```


***

## Part 10: Complete Practical Example

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define structures
typedef struct {
    int id;
    char name[^50];
    float gpa;
} Student;

typedef struct {
    Student* students;
    int count;
    int capacity;
} StudentList;

// Function prototypes
StudentList* createList(int capacity);
void addStudent(StudentList* list, int id, char name[], float gpa);
void printList(StudentList* list);
void freeList(StudentList* list);

// Implementations
StudentList* createList(int capacity) {
    StudentList* list = (StudentList*)malloc(sizeof(StudentList));
    list->students = (Student*)malloc(capacity * sizeof(Student));
    list->count = 0;
    list->capacity = capacity;
    return list;
}

void addStudent(StudentList* list, int id, char name[], float gpa) {
    if (list->count < list->capacity) {
        Student* s = &list->students[list->count];
        s->id = id;
        strcpy(s->name, name);
        s->gpa = gpa;
        list->count++;
    } else {
        printf("List is full!\n");
    }
}

void printList(StudentList* list) {
    printf("Student List:\n");
    for (int i = 0; i < list->count; i++) {
        Student* s = &list->students[i];
        printf("%d: %s (GPA: %.2f)\n", s->id, s->name, s->gpa);
    }
}

void freeList(StudentList* list) {
    free(list->students);
    free(list);
}

int main() {
    StudentList* list = createList(10);
    
    addStudent(list, 1, "Alice", 3.8f);
    addStudent(list, 2, "Bob", 3.5f);
    addStudent(list, 3, "Charlie", 3.9f);
    
    printList(list);
    
    freeList(list);
    
    return 0;
}
```

**Output:**

```
Student List:
1: Alice (GPA: 3.80)
2: Bob (GPA: 3.50)
3: Charlie (GPA: 3.90)
```


***

## Quick Reference: Structure Operations

| Operation | Syntax | Use Case |
| :-- | :-- | :-- |
| **Define struct** | `struct Name { ... };` | Create new data type |
| **Declare variable** | `struct Name var;` | Create instance |
| **Access member** | `var.member` | Get/set value |
| **Access via pointer** | `ptr->member` | Get/set via pointer |
| **Get address** | `&var` | Create pointer to struct |
| **Allocate** | `(Type*)malloc(sizeof(Type))` | Dynamic allocation |
| **Array of structs** | `struct Name arr[^10];` | Multiple instances |
| **Nested struct** | `struct Name { struct Other o; };` | Embed struct in struct |


***

**Practice Challenges:**

1. Create a `Book` struct with title, author, and ISBN. Create an array of books and print them.
2. Implement a simple address book using a struct for contact information.
3. Create a `Matrix` struct with pointer members and functions to add/multiply matrices.
4. Build a linked list node struct and create a simple linked list.
5. Compare the memory usage of a structure vs a union with the same members.

Which of these would you like to practice? Or should we move to the next topic (file handling)?
<span style="display:none">[^4][^5][^6][^7][^8][^9]</span>

<div align="center">⁂</div>

[^1]: https://www.programiz.com/c-programming/c-structures

[^2]: https://www.ccbp.in/blog/articles/difference-between-structure-and-union-in-c

[^3]: https://www.reddit.com/r/C_Programming/comments/b9ey9b/pointer_to_struct_vs_nested_struct/

[^4]: https://www.tutorialspoint.com/cprogramming/c_structures.htm

[^5]: https://www.codecademy.com/resources/docs/c/union

[^6]: https://beej.us/guide/bgc/html/split/structs-ii-more-fun-with-structs.html

[^7]: https://www.geeksforgeeks.org/c/structures-c/

[^8]: https://www.geeksforgeeks.org/c/structure-vs-union-in-c/

[^9]: https://c-for-dummies.com/blog/?p=2708

[^10]: https://www.educative.io/blog/how-to-use-the-typedef-struct-in-c

