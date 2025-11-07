#include <stdio.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024

int main() {
      FILE *file = fopen("data/2025/quest1.txt", "r");
      if (file == NULL) {
          perror("Error opening file");
          return 1;
      }

      char line[MAX_LINE_LENGTH];

      // Read line by line
      while (fgets(line, sizeof(line), file) != NULL) {
          // Remove newline character
          line[strcspn(line, "\n")] = '\0';

          printf("Line: %s\n", line);
      }

      fclose(file);
      return 0;
}
