/*
 * $Id$
 */

/* Language Support Module */

/* Language name: Basque */
/* ISO language code (2 chars): EU */
/* Codepage: ???? */

#include "hbdefs.h"

char *hb_monthsname[ 12 ] = 
{
   "Urtarrila",
   "Otsaila",
   "Martxoa",
   "Apirila",
   "Maitza",
   "Ekaina",
   "Uztaila",
   "Abuztua",
   "Iraila",
   "Urria",
   "Azaroa",
   "Abendua"
};

char *hb_daysname[ 7 ] =
{
   "Igandea",
   "Astelehena",
   "Asteartea",
   "Asteazkena",
   "Osteguna",
   "Ostirala",
   "Larunbata"
};

static char *genericErrors[] =
{
   "Unknown error",
   "Argument error",
   "Bound error",
   "String overflow",
   "Numeric overflow",
   "Divide by zero",
   "Numeric error",
   "Syntax error",
   "Operation too complex",
   "",
   "",
   "Memory low",
   "Undefined function",
   "No exported method",
   "Variable does not exists",
   "Alias does not exists",
   "No exported variable",
   "Incorrect alias name",
   "Duplicated alias name",
   "",
   "Create error",
   "Open error",
   "Close error",
   "Read error",
   "Write error",
   "Print error",
   "",
   "",
   "",
   "",
   "Unsupported operation",
   "Limit exeeded",
   "Index corruption detected",
   "Incorrect type of data",
   "Data width too long",
   "Workarea not in use",
   "Workarea not indexed",
   "Exclusive use required",
   "Lock required",
   "Write not allowed",
   "Append lock failed",
   "Lock failure",
   "",
   "",
   "",
   "Incorrect number of arguments",
   "array access",
   "array assign",
   "not an array",
   "conditional"
};
