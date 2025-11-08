#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_LINE_LENGTH 1024
#define MAX_STRINGS 100
#define MAX_INSTRUCTIONS 1000

typedef enum {
    L,
    R
} Direction;

typedef struct {
    Direction direction;
    int steps;
} Instruction;

int processInstructions(Instruction* instructions, int instruction_count, int names_length) {
    int accumulator = 0;
    
    for (int i = 0; i < instruction_count; i++) {
        if (instructions[i].direction == L) {
            accumulator -= instructions[i].steps;
            if (accumulator < 0) {
                accumulator = 0;
            }
        } else if (instructions[i].direction == R) {
            accumulator += instructions[i].steps;
            if (accumulator >= names_length) {
                accumulator = names_length - 1;
            }
        }
    }
    
    return accumulator;
}

int processInstructions2(Instruction* instructions, int instruction_count, int names_length) {
    int accumulator = 0;
    
    for (int i = 0; i < instruction_count; i++) {
        if (instructions[i].direction == L) {
            accumulator -= instructions[i].steps;
        } else if (instructions[i].direction == R) {
            accumulator += instructions[i].steps;
        }
    }
    
    return accumulator % names_length;
}

int main() {
    FILE *file = fopen("data/2025/quest2.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    char strings[MAX_STRINGS][MAX_LINE_LENGTH];
    Instruction instructions[MAX_INSTRUCTIONS];
    int string_count = 0;
    int instruction_count = 0;
    int line_num = 1;

    // Read line by line
    while (fgets(line, sizeof(line), file) != NULL) {
        // Remove newline character
        line[strcspn(line, "\n")] = '\0';

        if (line_num == 1) {
            // Parse comma-separated strings
            char *token = strtok(line, ",");
            while (token != NULL && string_count < MAX_STRINGS) {
                strcpy(strings[string_count], token);
                string_count++;
                token = strtok(NULL, ",");
            }
        } else if (line_num == 3) {
            // Parse instructions (L6,R1,L9,R7,...)
            char *token = strtok(line, ",");
            while (token != NULL && instruction_count < MAX_INSTRUCTIONS) {
                if (strlen(token) > 1) {
                    Direction dir = (token[0] == 'L') ? L : R;
                    int steps = atoi(&token[1]);
                    instructions[instruction_count].direction = dir;
                    instructions[instruction_count].steps = steps;
                    instruction_count++;
                }
                token = strtok(NULL, ",");
            }
        }
        line_num++;
    }

    // Print parsed data
    printf("Strings (%d):\n", string_count);
    for (int i = 0; i < string_count; i++) {
        printf("  %d: %s\n", i, strings[i]);
    }

    printf("\nInstructions (%d):\n", instruction_count);
    for (int i = 0; i < instruction_count; i++) {
        printf("  %d: %c%d\n", i, 
               (instructions[i].direction == L) ? 'L' : 'R', 
               instructions[i].steps);
    }

    int final_position = processInstructions2(instructions, instruction_count, string_count);
    printf("\nFinal position: %d\n", final_position);
    if (final_position < string_count) {
        printf("Selected name: %s\n", strings[final_position]);
    }

    fclose(file);
    return 0;
}
