/*
 * $Id$
 */

#include "hbdefs.h"

char *hb_monthsname[ 12 ] = {
   "Januar", "Februar", "März",
   "April", "Mai", "Juni", "Juli",
   "August", "September", "Oktober",
   "November", "Dezember" };

char *hb_daysname[ 7 ] = {
   "Sonntag", "Montag", "Dienstag",
   "Mittwoch", "Donnerstag", "Freitag",
   "Samstag" };

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

char *hb_ErrorNatDescription( ULONG ulGenError )
{
   if( ulGenError < sizeof(genericErrors)/sizeof(char*) )
      return genericErrors[ ulGenError ];
   else
      return genericErrors[ 0 ];
}
