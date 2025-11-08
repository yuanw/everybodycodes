#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_LINE_LENGTH 1024
#define MAX_STRINGS 100
#define MAX_INSTRUCTIONS 1000


typedef struct {
    int x;
    int y;
} Complex;

Complex add(Complex a, Complex b) {
  return Complex(a.x + b.x, a.y + b.y);
}

int main() {
    FILE *file = fopen("data/2025/quest1-part3.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fclose(file);
    return 0;
}
