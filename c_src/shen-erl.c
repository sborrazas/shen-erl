#include <stdio.h> /* printf */
#include <stdlib.h> /* malloc, free */
#include <string.h> /* strcpy, strcmp */
#include <unistd.h> /* execvp */
#include <stdarg.h> /* va_list, va_start, va_end */
#include <errno.h> /* errno */

#define PUSH(s) Eargv[Eargc++] = s
#define PROGNAME "erl"

/* Local functions */
static void error(char *format, ...);

int main(int argc, char **argv)
{
  char **Eargv = NULL;
  int Eargc = 0;
  char* rootdir;
  char* path;
  char* arg;
  int i = 1;
  int eval = 0;

  rootdir = getenv("SHEN_ERL_ROOTDIR");

  if (rootdir == NULL) {
    error("SHEN_ERL_ROOTDIR environment variable is not set");
  }
  path = malloc(strlen(rootdir) + 6);
  sprintf(path, "%s/ebin", rootdir);

  Eargv = (char **)malloc(sizeof(*argv) * (argc + 16));
  Eargc = 0;
  PUSH(PROGNAME);			/* The program we are going to run */

  PUSH("-pa");
  PUSH(path);

  while (i < argc) {
    arg = argv[i];
    if (strcmp(arg, "-eval") == 0) {
      eval = 1;
      i += 1;
      break;
    }
    else { /* Plain argument. */
      break;
    }
  }
  PUSH("-noshell");

  /* Running shen_erl_init */
  PUSH("-s");
  PUSH("shen_erl_init");

  PUSH("-extra"); /* Program arguments */

  /* Check if we are 'eval'ing and add -eval flag */
  if (eval) {
    PUSH("-eval");
  }
  /* Add the rest to the stack and terminate it. */
  while (i < argc) {
    PUSH(argv[i++]);
  }

  for (i = 0; i < Eargc; i++) {
    printf("%s ", Eargv[i]);
  }
  printf("\n");

  Eargv[Eargc] = NULL;

  execvp(PROGNAME, Eargv);

  error("Error %d executing '%s'", errno, PROGNAME);
}

void error(char* format, ...)
{
  char sbuf[1024];
  va_list ap;

  va_start(ap, format);
  snprintf(sbuf, sizeof(sbuf), format, ap);
  va_end(ap);
  fprintf(stderr, "shen-erl: %s\n", sbuf);
  exit(1);
}
