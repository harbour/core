/*
 * $Id$
 */

/***
* Function: _GT_Internal_StringAsInt()
* Syntax..: int _GT_Internal_StringAsInt(char *String, int Start, int End)
* Usage...: Convert a numeric value in a string to an int value.
* By......: David A Pearson
*/

#include "hbapi.h"

int _GT_Internal_StringAsInt( char * String, HB_ISIZ Start, HB_ISIZ End )
{
   int     Decimal = 1;
   int     Value   = 0;
   HB_ISIZ Digit;

   HB_TRACE( HB_TR_DEBUG, ( "_GT_Internal_StringAsInt(%s, %" HB_PFS "d, %" HB_PFS "d)", String, Start, End ) );

   for( Digit = End; Digit >= Start; Digit-- )
   {
      if( HB_ISDIGIT( String[ Digit ] ) )
      {
         Value   += ( String[ Digit ] - '0' ) * Decimal;
         Decimal *= 10;
      }
   }

   return Value;
}
