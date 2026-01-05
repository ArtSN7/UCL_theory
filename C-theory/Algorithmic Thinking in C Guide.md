

# Algorithmic Thinking in C: Comprehensive Guide

This guide covers fundamental algorithms (sorting, searching), text processing (word wrapping, column management), and specialized data structures (ragged arrays).

***

## Part 1: Sorting Algorithms

### Bubble Sort

**Concept:** Repeatedly steps through the array, compares adjacent elements, and swaps them if they're in the wrong order. The largest element "bubbles up" to the end in each pass.

**How It Works:**

1. Compare first two elements, swap if needed
2. Move to next pair, repeat
3. After first pass, largest element is at the end
4. Repeat for remaining elements (excluding already sorted ones)
5. Continue until no swaps are needed

**Time Complexity:**

- Best: O(n) - already sorted
- Average: O(n²)
- Worst: O(n²)

**Space Complexity:** O(1) - in-place sorting

**Implementation:**

```c
#include <stdio.h>

void bubbleSort(int arr[], int n) {
    int i, j, temp;
    int swapped;
    
    // Outer loop: n-1 passes
    for (i = 0; i < n - 1; i++) {
        swapped = 0;  // Optimization: track if swap occurred
        
        // Inner loop: compare adjacent elements
        // Reduce range each pass (last i elements already sorted)
        for (j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                // Swap elements
                temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
                swapped = 1;
            }
        }
        
        // If no swaps occurred, array is sorted
        if (!swapped) {
            break;
        }
    }
}

void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[^0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    bubbleSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

**Output:**

```
Original array: 64 34 25 12 22 11 90
Sorted array: 11 12 22 25 34 64 90
```

**Step-by-Step Example:**

```
Initial: [64, 34, 25, 12, 22, 11, 90]

Pass 1:
[34, 64, 25, 12, 22, 11, 90]  (swap 64 and 34)
[34, 25, 64, 12, 22, 11, 90]  (swap 64 and 25)
[34, 25, 12, 64, 22, 11, 90]  (swap 64 and 12)
[34, 25, 12, 22, 64, 11, 90]  (swap 64 and 22)
[34, 25, 12, 22, 11, 64, 90]  (swap 64 and 11)
[34, 25, 12, 22, 11, 64, 90]  (64 < 90, no swap)

Pass 2:
[25, 34, 12, 22, 11, 64, 90]
[25, 12, 34, 22, 11, 64, 90]
[25, 12, 22, 34, 11, 64, 90]
[25, 12, 22, 11, 34, 64, 90]
...and so on
```


***

### Selection Sort

**Concept:** Divide array into sorted and unsorted parts. Repeatedly find the minimum element from unsorted part and place it at the beginning.

**How It Works:**

1. Find minimum element in entire array
2. Swap it with first element
3. Find minimum in remaining array (excluding first element)
4. Swap with second position
5. Repeat until array is sorted

**Time Complexity:**

- Best: O(n²)
- Average: O(n²)
- Worst: O(n²)

**Space Complexity:** O(1)

**Implementation:**

```c
#include <stdio.h>

void selectionSort(int arr[], int n) {
    int i, j, min_idx, temp;
    
    // Move boundary of unsorted portion
    for (i = 0; i < n - 1; i++) {
        // Find minimum element in unsorted array
        min_idx = i;
        
        for (j = i + 1; j < n; j++) {
            if (arr[j] < arr[min_idx]) {
                min_idx = j;
            }
        }
        
        // Swap found minimum with first element of unsorted part
        if (min_idx != i) {
            temp = arr[i];
            arr[i] = arr[min_idx];
            arr[min_idx] = temp;
        }
    }
}

int main() {
    int arr[] = {64, 25, 12, 22, 11};
    int n = sizeof(arr) / sizeof(arr[^0]);
    
    printf("Original array: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    selectionSort(arr, n);
    
    printf("Sorted array: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    return 0;
}
```

**Output:**

```
Original array: 64 25 12 22 11
Sorted array: 11 12 22 25 64
```

**Step-by-Step Example:**

```
Initial: [64, 25, 12, 22, 11]

Pass 1: Find min (11), swap with first
[11, 25, 12, 22, 64]

Pass 2: Find min in [25, 12, 22, 64] -> 12, swap with 25
[11, 12, 25, 22, 64]

Pass 3: Find min in [25, 22, 64] -> 22, swap with 25
[11, 12, 22, 25, 64]

Pass 4: Find min in [25, 64] -> 25, no swap needed
[11, 12, 22, 25, 64]
```


***

## Part 2: Searching Algorithms

### Linear Search

**Concept:** Search for an element by checking each element sequentially from start to end.

**How It Works:**

1. Start from first element
2. Compare with target
3. If match found, return index
4. If not, move to next element
5. If reach end without finding, return -1

**Time Complexity:**

- Best: O(1) - found at first position
- Average: O(n)
- Worst: O(n) - element at end or not present

**Space Complexity:** O(1)

**Implementation:**

```c
#include <stdio.h>

int linearSearch(int arr[], int n, int target) {
    for (int i = 0; i < n; i++) {
        if (arr[i] == target) {
            return i;  // Return index where found
        }
    }
    return -1;  // Not found
}

int main() {
    int arr[] = {10, 23, 45, 70, 11, 15};
    int n = sizeof(arr) / sizeof(arr[^0]);
    int target = 70;
    
    int result = linearSearch(arr, n, target);
    
    if (result == -1) {
        printf("Element %d not found\n", target);
    } else {
        printf("Element %d found at index %d\n", target, result);
    }
    
    return 0;
}
```

**Output:**

```
Element 70 found at index 3
```


***

### Binary Search

**Concept:** Efficiently search in a **sorted array** by repeatedly dividing the search interval in half.

**Prerequisite:** Array must be sorted!

**How It Works:**

1. Find middle element
2. If target equals middle, return index
3. If target is less than middle, search left half
4. If target is greater than middle, search right half
5. Repeat until found or search space is empty

**Time Complexity:**

- Best: O(1) - found at middle
- Average: O(log n)
- Worst: O(log n)

**Space Complexity:** O(1) - iterative version

**Implementation (Iterative):**

```c
#include <stdio.h>

int binarySearch(int arr[], int n, int target) {
    int low = 0;
    int high = n - 1;
    
    while (low <= high) {
        int mid = low + (high - low) / 2;  // Avoid overflow
        
        if (arr[mid] == target) {
            return mid;  // Found
        }
        
        if (arr[mid] < target) {
            low = mid + 1;  // Search right half
        } else {
            high = mid - 1;  // Search left half
        }
    }
    
    return -1;  // Not found
}

int main() {
    int arr[] = {2, 3, 4, 10, 40, 50, 60};
    int n = sizeof(arr) / sizeof(arr[^0]);
    int target = 10;
    
    int result = binarySearch(arr, n, target);
    
    if (result == -1) {
        printf("Element %d not found\n", target);
    } else {
        printf("Element %d found at index %d\n", target, result);
    }
    
    return 0;
}
```

**Output:**

```
Element 10 found at index 3
```

**Step-by-Step Example:**

```
Array: [2, 3, 4, 10, 40, 50, 60], Target: 10

Step 1: low=0, high=6, mid=3
arr[^3] = 10 == target → Found!
```

**Another Example (Not Found):**

```
Array: [2, 3, 4, 10, 40, 50, 60], Target: 15

Step 1: low=0, high=6, mid=3
arr[^3] = 10 < 15 → Search right: low=4

Step 2: low=4, high=6, mid=5
arr[^5] = 50 > 15 → Search left: high=4

Step 3: low=4, high=4, mid=4
arr[^4] = 40 > 15 → Search left: high=3

Step 4: low=4, high=3 (low > high) → Not found
```


***

## Part 3: Text Wrapping and Column Management

### Basic Word Wrapping Algorithm

**Problem:** Given text and a maximum line width, wrap text without breaking words.

**Algorithm:**

1. Split text into words
2. Build lines by adding words one at a time
3. If adding a word exceeds width, start new line
4. Continue until all words are placed

**Implementation:**

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void wrapText(char* text, int maxWidth) {
    char* token;
    char* textCopy = strdup(text);  // Make a copy
    int currentLineLength = 0;
    
    token = strtok(textCopy, " ");
    
    while (token != NULL) {
        int wordLength = strlen(token);
        
        // Check if adding this word would exceed width
        if (currentLineLength + wordLength + (currentLineLength > 0 ? 1 : 0) > maxWidth) {
            printf("\n");  // Start new line
            currentLineLength = 0;
        }
        
        // Add space before word (except at start of line)
        if (currentLineLength > 0) {
            printf(" ");
            currentLineLength++;
        }
        
        printf("%s", token);
        currentLineLength += wordLength;
        
        token = strtok(NULL, " ");
    }
    
    printf("\n");
    free(textCopy);
}

int main() {
    char text[] = "This is a sample text that needs to be wrapped at a specific width";
    int maxWidth = 20;
    
    printf("Wrapping text at width %d:\n", maxWidth);
    wrapText(text, maxWidth);
    
    return 0;
}
```

**Output:**

```
Wrapping text at width 20:
This is a sample
text that needs to
be wrapped at a
specific width
```


***

### Advanced: Column Management (Two Columns)

**Problem:** Format text into two columns of specified width.

**Implementation:**

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_LINES 100
#define MAX_LINE_LENGTH 100

typedef struct {
    char lines[MAX_LINES][MAX_LINE_LENGTH];
    int count;
} Column;

void wrapToColumn(char* text, int columnWidth, Column* col) {
    char* token;
    char* textCopy = strdup(text);
    char currentLine[MAX_LINE_LENGTH] = "";
    int currentLength = 0;
    
    col->count = 0;
    token = strtok(textCopy, " ");
    
    while (token != NULL) {
        int wordLen = strlen(token);
        
        // Check if word fits on current line
        if (currentLength + wordLen + (currentLength > 0 ? 1 : 0) > columnWidth) {
            // Save current line and start new one
            strcpy(col->lines[col->count++], currentLine);
            strcpy(currentLine, token);
            currentLength = wordLen;
        } else {
            // Add word to current line
            if (currentLength > 0) {
                strcat(currentLine, " ");
                currentLength++;
            }
            strcat(currentLine, token);
            currentLength += wordLen;
        }
        
        token = strtok(NULL, " ");
    }
    
    // Save last line
    if (currentLength > 0) {
        strcpy(col->lines[col->count++], currentLine);
    }
    
    free(textCopy);
}

void printTwoColumns(Column* col1, Column* col2, int colWidth) {
    int maxLines = col1->count > col2->count ? col1->count : col2->count;
    
    for (int i = 0; i < maxLines; i++) {
        // Print first column
        if (i < col1->count) {
            printf("%-*s", colWidth, col1->lines[i]);
        } else {
            printf("%-*s", colWidth, "");  // Empty space
        }
        
        printf("  |  ");  // Column separator
        
        // Print second column
        if (i < col2->count) {
            printf("%s", col2->lines[i]);
        }
        
        printf("\n");
    }
}

int main() {
    char text[] = "This is a longer text that will be split into two columns "
                  "to demonstrate column-based text formatting which is useful "
                  "for creating newspaper-style layouts or side-by-side comparisons";
    
    int columnWidth = 30;
    Column col1, col2;
    
    // Split text in half for demonstration
    char firstHalf[^500], secondHalf[^500];
    int halfPoint = strlen(text) / 2;
    
    // Find space near middle
    while (halfPoint < strlen(text) && text[halfPoint] != ' ') {
        halfPoint++;
    }
    
    strncpy(firstHalf, text, halfPoint);
    firstHalf[halfPoint] = '\0';
    strcpy(secondHalf, text + halfPoint + 1);
    
    wrapToColumn(firstHalf, columnWidth, &col1);
    wrapToColumn(secondHalf, columnWidth, &col2);
    
    printf("Two-Column Layout:\n");
    printf("================================\n");
    printTwoColumns(&col1, &col2, columnWidth);
    
    return 0;
}
```

**Output:**

```
Two-Column Layout:
================================
This is a longer text that      |  demonstrate column-based
will be split into two           |  text formatting which is
columns to                       |  useful for creating
                                |  newspaper-style layouts
                                |  or side-by-side
                                |  comparisons
```


***

## Part 4: Ragged Arrays (Varying Row Lengths)

### Concept

A **ragged array** (or jagged array) is a 2D array where each row can have a different length.

### Creating a Ragged Array

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int** data;
    int rows;
    int* colSizes;
} RaggedArray;

RaggedArray* createRaggedArray(int rows, int colSizes[]) {
    RaggedArray* arr = (RaggedArray*)malloc(sizeof(RaggedArray));
    
    arr->rows = rows;
    arr->colSizes = (int*)malloc(rows * sizeof(int));
    arr->data = (int**)malloc(rows * sizeof(int*));
    
    for (int i = 0; i < rows; i++) {
        arr->colSizes[i] = colSizes[i];
        arr->data[i] = (int*)malloc(colSizes[i] * sizeof(int));
    }
    
    return arr;
}

void fillRaggedArray(RaggedArray* arr) {
    int value = 1;
    for (int i = 0; i < arr->rows; i++) {
        for (int j = 0; j < arr->colSizes[i]; j++) {
            arr->data[i][j] = value++;
        }
    }
}

void printRaggedArray(RaggedArray* arr) {
    for (int i = 0; i < arr->rows; i++) {
        printf("Row %d (%d elements): ", i, arr->colSizes[i]);
        for (int j = 0; j < arr->colSizes[i]; j++) {
            printf("%d ", arr->data[i][j]);
        }
        printf("\n");
    }
}

void freeRaggedArray(RaggedArray* arr) {
    for (int i = 0; i < arr->rows; i++) {
        free(arr->data[i]);
    }
    free(arr->data);
    free(arr->colSizes);
    free(arr);
}

int main() {
    int colSizes[] = {3, 5, 2, 4};
    RaggedArray* arr = createRaggedArray(4, colSizes);
    
    fillRaggedArray(arr);
    printRaggedArray(arr);
    freeRaggedArray(arr);
    
    return 0;
}
```

**Output:**

```
Row 0 (3 elements): 1 2 3
Row 1 (5 elements): 4 5 6 7 8
Row 2 (2 elements): 9 10
Row 3 (4 elements): 11 12 13 14
```


### Operations on Ragged Arrays

**Get Element:**

```c
int getElement(RaggedArray* arr, int row, int col) {
    if (row < 0 || row >= arr->rows) {
        fprintf(stderr, "Row index out of bounds\n");
        return -1;
    }
    if (col < 0 || col >= arr->colSizes[row]) {
        fprintf(stderr, "Column index out of bounds for row %d\n", row);
        return -1;
    }
    return arr->data[row][col];
}
```

**Set Element:**

```c
void setElement(RaggedArray* arr, int row, int col, int value) {
    if (row >= 0 && row < arr->rows && col >= 0 && col < arr->colSizes[row]) {
        arr->data[row][col] = value;
    } else {
        fprintf(stderr, "Invalid indices\n");
    }
}
```

**Add Row:**

```c
void addRow(RaggedArray* arr, int newRowSize) {
    // Resize arrays
    arr->rows++;
    arr->data = (int**)realloc(arr->data, arr->rows * sizeof(int*));
    arr->colSizes = (int*)realloc(arr->colSizes, arr->rows * sizeof(int));
    
    // Add new row
    int newIdx = arr->rows - 1;
    arr->colSizes[newIdx] = newRowSize;
    arr->data[newIdx] = (int*)calloc(newRowSize, sizeof(int));
}
```


***

## Quick Reference: Algorithm Comparison

| Algorithm | Type | Time Complexity (Avg) | Space | Best For |
| :-- | :-- | :-- | :-- | :-- |
| **Bubble Sort** | Sorting | O(n²) | O(1) | Small datasets, educational |
| **Selection Sort** | Sorting | O(n²) | O(1) | Small datasets, minimal swaps |
| **Linear Search** | Searching | O(n) | O(1) | Unsorted data |
| **Binary Search** | Searching | O(log n) | O(1) | Sorted data |


***

## Complete Practical Example: Sorting and Searching

```c
#include <stdio.h>
#include <stdlib.h>

// Function prototypes
void bubbleSort(int arr[], int n);
int binarySearch(int arr[], int n, int target);
void printArray(int arr[], int n);

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90, 5};
    int n = sizeof(arr) / sizeof(arr[^0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    // Sort the array
    bubbleSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    // Search for elements
    int targets[] = {22, 100, 5};
    for (int i = 0; i < 3; i++) {
        int result = binarySearch(arr, n, targets[i]);
        if (result != -1) {
            printf("Element %d found at index %d\n", targets[i], result);
        } else {
            printf("Element %d not found\n", targets[i]);
        }
    }
    
    return 0;
}

void bubbleSort(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        int swapped = 0;
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
                swapped = 1;
            }
        }
        if (!swapped) break;
    }
}

int binarySearch(int arr[], int n, int target) {
    int low = 0, high = n - 1;
    while (low <= high) {
        int mid = low + (high - low) / 2;
        if (arr[mid] == target) return mid;
        if (arr[mid] < target) low = mid + 1;
        else high = mid - 1;
    }
    return -1;
}

void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}
```

**Output:**

```
Original array: 64 34 25 12 22 11 90 5
Sorted array: 5 11 12 22 25 34 64 90
Element 22 found at index 3
Element 100 not found
Element 5 found at index 0
```


***

**Practice Challenges:**

1. Implement insertion sort and compare it with bubble sort.
2. Create a function that uses binary search to find all occurrences of a value.
3. Write a text formatter that justifies text (adds spaces to make lines equal width).
4. Build a ragged array that stores a triangle of numbers (Pascal's triangle).
5. Create a word-wrap function that handles hyphenation for long words.

Which algorithm or concept would you like to explore deeper, or are you ready to start working on your original exercises?
<span style="display:none">[^1][^10][^11][^12][^13][^14][^15][^16][^17][^18][^19][^2][^20][^6][^7][^8][^9]</span>

<div align="center">⁂</div>

[^1]: https://www.w3schools.com/dsa/dsa_algo_bubblesort.php

[^2]: https://www.geeksforgeeks.org/dsa/searching-algorithms/

[^3]: https://stackoverflow.com/questions/22582989/word-wrap-program-c

[^4]: https://www.programiz.com/dsa/bubble-sort

[^5]: https://www.geeksforgeeks.org/dsa/linear-search-vs-binary-search/

[^6]: https://mike-ward.net/2009/07/19/word-wrap-in-a-console-app-c/

[^7]: https://www.geeksforgeeks.org/dsa/selection-sort-vs-bubble-sort/

[^8]: https://www.freecodecamp.org/news/search-algorithms-linear-and-binary-search-explained/

[^9]: https://en.wikipedia.org/wiki/Wrapping_(text)

[^10]: https://visualgo.net/en/sorting

[^11]: https://pmt.physicsandmathstutor.com/download/Computer-Science/A-level/Notes/OCR/2.3-Algorithms-AS/Advanced/2.3.4. Searching Algorithms.pdf

[^12]: https://www.geeksforgeeks.org/dsa/word-wrap-problem-dp-19/

[^13]: https://workat.tech/problem-solving/tutorial/sorting-algorithms-bubble-insertion-selection-sort-veubp86w3e1r

[^14]: https://www.bbc.co.uk/bitesize/guides/zm77xfr/revision/4

[^15]: https://forum.xojo.com/t/word-wrap-algorithm/64400

[^16]: https://github.com/nickssilver/sorting_algorithms

[^17]: https://www.luigisbox.com/blog/types-of-search-algorithms/

[^18]: https://phramebuffer.wordpress.com/rendering-text-and-text-wrapping-algorithm/

[^19]: https://www.geeksforgeeks.org/dsa/sorting-algorithms/

[^20]: https://www.programiz.com/dsa/binary-search

