/*************************************************************************\
* Copyright (c) 2002 The University of Chicago, as Operator of Argonne
* National Laboratory.
* Copyright (c) 2002 The Regents of the University of California, as
* Operator of Los Alamos National Laboratory.
* This file is distributed subject to a Software License Agreement found
* in the file LICENSE that is included with this distribution. 
\*************************************************************************/
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
