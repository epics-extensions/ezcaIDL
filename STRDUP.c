/* This file contains strdup() for VMS, since the standard C run time library
   does not contain one */
#include <stdlib.h>
#include <string.h>
char *strdup(char *in)
{
	char *t;
	t = malloc(strlen(in)+1);
	if (t != NULL) strcpy(t, in);
	return(t);
}
