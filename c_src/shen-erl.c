#include <stdio.h> /* printf */
#include <stdlib.h> /* malloc, free */
#include <string.h> /* strcpy */
#include <unistd.h> /* execvp */

#define NUM_ARGS 7
#define MAX_ARGS_SIZE 100

int main(int argc, char **argv) {
  char* const Eargv[] = {"erl", "-noshell", "-pa", "ebin", "-s", "shen_erl_init", "-extra", "example.kl", NULL};

  execvp("erl", Eargv);
}
