// The MIT License (MIT)
// 
// Copyright (c) 2024 Trevor Bakker 
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.


#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#define MAX_INPUT 255

// Error handling function
void handle_error() {
    char error_message[30] = "An error has occurred\n";
    write(STDERR_FILENO, error_message, strlen(error_message));
}

// Function to execute commands
void execute_command(char *line) {
    char *command_args[MAX_INPUT / 2 + 1];
    char *token = strtok(line, " ");
    int i = 0;
    
    // Tokenize the input string
    while (token != NULL) {
        command_args[i++] = token;
        token = strtok(NULL, " ");
    }
    command_args[i] = NULL; // Null-terminate the array

    // Handle cd command
    if (strcmp(command_args[0], "cd") == 0) {
        if (command_args[1] == NULL) {
            command_args[1] = "/workspaces/shell-assignment-edencse/msh"; // Default directory
        }
        if (chdir(command_args[1]) != 0) {
            handle_error(); // Use handle_error for error message
        }
        return; // Skip command execution for cd
    }

    // Check for output redirection
    int redirect_out = 0;
    char *output_file = NULL;
    for (int j = 0; j < i; j++) {
        if (strcmp(command_args[j], ">") == 0) {
            redirect_out = 1;
            output_file = command_args[j + 1];
            command_args[j] = NULL; // Null-terminate the command
            break;
        }
    }

    // Ensure an output file is provided if redirection is requested
    if (redirect_out && output_file == NULL) {
        handle_error(); // Use handle_error for missing output file error
        return;
    }

    // Fork a new process
    pid_t pid = fork();
    
    if (pid < 0) {
        handle_error(); // Use handle_error for fork failure
    } else if (pid == 0) {
        // Child process
        if (redirect_out) {
            int fd = open(output_file, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
            if (fd < 0) {
                handle_error(); // Use handle_error for file open failure
                exit(EXIT_FAILURE);
            }
            dup2(fd, STDOUT_FILENO); // Redirect stdout to the file
            close(fd);
        }

        execvp(command_args[0], command_args); // Execute the command
        handle_error(); // Exec only returns on failure
        exit(EXIT_FAILURE); // Exit child process if exec fails
    } else {
        // Parent process
        wait(NULL); // Wait for the child process to finish
    }
}

int main(int argc, char *argv[]) {
    if (argc == 2) {
        // Batch mode
        char line[MAX_INPUT];
        FILE *file = fopen(argv[1], "r");
        
        if (file == NULL) {
            handle_error(); // Use handle_error for file opening failure
            return 1; // Exit if the file can't be opened
        }

        // Read commands from the file
        while (fgets(line, sizeof(line), file)) {
            line[strcspn(line, "\n")] = 0; // Remove newline

            if (strcmp(line, "\n") == 0 || strcmp(line, "") == 0) 
                continue; // Ignore empty input

            if (strcmp(line, "exit") == 0 || strcmp(line, "quit") == 0) {
                break; // Exit the shell
            }

            execute_command(line); // Process and execute the command
        }

        fclose(file); // Close the file after reading
    } else {
        // Interactive mode
        while (1) {
            printf("msh> ");
            
            char input[MAX_INPUT];
            if (fgets(input, sizeof(input), stdin) == NULL) {
                handle_error(); // Use handle_error for fgets failure
                continue; // Handle EOF or error
            }

            input[strcspn(input, "\n")] = 0; // Remove newline

            if (strcmp(input, "\n") == 0 || strcmp(input, "") == 0) 
                continue; // Ignore empty input

            if (strcmp(input, "exit") == 0 || strcmp(input, "quit") == 0) {
                break; // Exit the shell
            }

            execute_command(input); // Process and execute the command
        }
    }

    return 0;
}


