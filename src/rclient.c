#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

#include <assert.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>

#define BUF_SIZE 1000000 /* The String buffer size */

#define NSTRS       3           /* no. of strings  */
#define ADDRESS     "mysocket"  /* addr to connect */

/* 
   This code adapted from: 
http://www.cs.cf.ac.uk/Dave/C/node28.html#SECTION002862000000000000000

Original author: Dave Marshall

Adapted for use with R by: Balasubramanian Narasimhan (major) and Tomer Altman (minor)

 */

/*
 * Strings we send to the server.
 */
char *strs[NSTRS] = {
    "This is the first string from the client.\n",
    "This is the second string from the client.\n",
    "This is the third string from the client.\n"
};


SEXP clientSocket(SEXP sAddress, SEXP sMessages) {
  char buf[BUF_SIZE], c;
  FILE *fp;
  register int i, s, len, nStr;
  struct sockaddr_un saun;
  SEXP result;

  /*
   * Get a socket to work with.  This socket will
   * be in the UNIX domain, and will be a
   * stream socket.
   */
  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
    error("readSocket: Unable to create socket");
  }

  /*
   * Create the address we will be connecting to.
   */
  strcpy(saun.sun_path, CHAR(STRING_ELT(sAddress, 0)));
  /* printf("address is: %s\n", saun.sun_path); */

  saun.sun_family = AF_UNIX;

  /*
   * Try to connect to the address.  For this to
   * succeed, the server must already have bound
   * this address, and must have issued a listen()
   * request.
   *
   * The third argument indicates the "length" of
   * the structure, not just the length of the
   * socket name.
   */
  len = sizeof(saun.sun_family) + strlen(saun.sun_path);

  if (connect(s, &saun, len) < 0) {
    error("readsocket: unable to connect to socket");
  }

  /*
   * We'll use stdio for reading
   * the socket.
   */
  fp = fdopen(s, "r");



  nStr = LENGTH(sMessages);  /* Same number in and out */

  PROTECT(result = allocVector(STRSXP, nStr));

  /*
   * We send some strings to the server.
   */
  for (i = 0; i < nStr; i++) {
    strcpy(buf, CHAR(STRING_ELT(sMessages, i)));
    send(s, buf, strlen(CHAR(STRING_ELT(sMessages,i))), 0);
  }


  /*
   * Now we read some strings from the server
   * and print them out.
   */

  /*  for (i = 0; i < nStr; i++) { */
  i = 0;
    int j = 0;
    while ((c = fgetc(fp)) != EOF) {
      /* putchar(c); *//* I think this statement is causing the text output... */
      buf[j] = c;
      j++;
      if (c == '\n' || j > BUF_SIZE) {
	SET_STRING_ELT(result, i, Rf_mkCharLen(buf, j));
	j = 0;	
	i++;
      }
    }

    /*  } */


  /*
   * We can simply use close() to terminate the
   * connection, since we're done with both sides.
   */
  close(s);
  UNPROTECT(1);
  return result;
}


