/*
 * $Id$
 */

/*****************************************************************************
* Function: _GT_Internal_StringAsInt()                                       *
* Syntax..: int _GT_Internal_StringAsInt(char *String, int Start, int End)   *
* Usage...: Convert a numeric value in a string to an int value.             *
* By......: David A Pearson                                                  *
*****************************************************************************/

#include "hbtrace.h"

#define ISDIGIT(c)      ((c) >= '0' && (c) <= '9')

int _GT_Internal_StringAsInt(char *String, int Start, int End)
{
   int  Decimal = 1;
   int  Digit;
   int  Value   = 0;

   HB_TRACE(HB_TR_DEBUG, ("_GT_Internal_StringAsInt(%s, %d, %d)", String, Start, End));
   
   for (Digit = End; Digit >= Start; Digit--)
     {
       if (ISDIGIT(String[Digit]))
         {
           Value   += (String[Digit] - 0x30) * Decimal;
           Decimal *= 0xA;
         }
     }
   return(Value);
}
