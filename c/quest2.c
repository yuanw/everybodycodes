#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_LINE_LENGTH 1024
#define MAX_STRINGS 100
#define MAX_INSTRUCTIONS 1000


typedef struct {
    int x;
    int y;
} Complex;

Complex add(Complex a, Complex b) {
  return (Complex){a.x + b.x, a.y + b.y};
}

Complex multiply(Complex a, Complex b) {
  return (Complex){a.x * b.x - a.y * b.y, a.x*b.y + a.y *b.x};
}

Complex divide(Complex a, Complex b) {
  return (Complex){a.x / b.x, a.y/b.y};
}

Complex cycle(Complex r, Complex a) {
  r = multiply(r,r);
  r = divide(r, (Complex){10,10});
  return add(r, a);
}

Complex partI(Complex r, Complex a) {
  r = cycle(r, a);
  r = cycle(r, a);
  return cycle(r,a);
}

// Unit tests for add function
void test_add_positive_numbers() {
    Complex a = {1, 1};
    Complex b = {2, 2};
    Complex result = add(a, b);
    assert(result.x == 3 && result.y == 3);
    printf("✓ test_add_positive_numbers passed\n");
}

void test_add_negative_numbers() {
    Complex a = {-3, -5};
    Complex b = {-2, -7};
    Complex result = add(a, b);
    assert(result.x == -5 && result.y == -12);
    printf("✓ test_add_negative_numbers passed\n");
}

void test_add_mixed_numbers() {
    Complex a = {10, -5};
    Complex b = {-3, 8};
    Complex result = add(a, b);
    assert(result.x == 7 && result.y == 3);
    printf("✓ test_add_mixed_numbers passed\n");
}

void test_add_zero() {
    Complex a = {5, 10};
    Complex b = {0, 0};
    Complex result = add(a, b);
    assert(result.x == 5 && result.y == 10);
    printf("✓ test_add_zero passed\n");
}

void test_add_identity() {
    Complex a = {0, 0};
    Complex b = {0, 0};
    Complex result = add(a, b);
    assert(result.x == 0 && result.y == 0);
    printf("✓ test_add_identity passed\n");
}

/* [10,12] / [2,2] = [10 / 2, 12 / 2] = [5,6] */
/* [11,12] / [3,5] = [11 / 3, 12 / 5] = [3,2] */
/* [-10,-12] / [2,2] = [-10 / 2, -12 / 2] = [-5,-6] */
/* [-11,-12] / [3,5] = [-11 / 3, -12 / 5] = [-3,-2]   */

void test_division() {
  Complex x = {10, 12};
  Complex y = {2,2};
  Complex result = divide(x,y);
  assert(result.x == 5 && result.y == 6);
  x = (Complex){11, 12};
  y = (Complex){3,5};
  result = divide(x,y);
  assert(result.x == 3 && result.y == 2);
  x = (Complex){-10, -12};
  y = (Complex){2,2};
  result = divide(x,y);
  assert(result.x == -5 && result.y == -6);
  
  x = (Complex){-11, -12};
  y = (Complex){3,5};
  result = divide(x,y);
  assert(result.x == -3 && result.y == -2);
 
  printf("✓ test_division passed\n");
}

void test_cycle() {
  Complex r = {0,0};
  Complex a = {25,9};
  Complex result = cycle(r,a);
  assert(result.x == 25 && result.y ==9);
  result = cycle(result, a);
  assert(result.x == 79 && result.y ==54);

  result = cycle(result, a);
  assert(result.x == 357 && result.y ==862);
  
  printf("✓ test_cycle passed\n");
}

void run_tests() {
    printf("Running unit tests for add function...\n");
    test_add_positive_numbers();
    test_add_negative_numbers();
    test_add_mixed_numbers();
    test_add_zero();
    test_add_identity();
    test_division();
    test_cycle();
    printf("All tests passed!\n\n");
}



int main() {
    // Run unit tests first
    run_tests();
    FILE *file = fopen("data/2025/quest2/test.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fclose(file);
    Complex result = partI((Complex){0,0}, (Complex){160,51});
    printf("\n[%d,%d]\n", result.x, result.y);
    
    return 0;
}
