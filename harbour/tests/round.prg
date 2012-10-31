/*
 * $Id$
 */

// Testing Harbour rounding.
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

PROCEDURE Main()

   LOCAL n, value := -5

   FOR n := 1 TO 100
      ? ;
         value, ;
         Round( value, 3 ),;
         Round( value, 2 ),;
         Round( value, 1 ),;
         Round( value, 0 )
      value += 0.001
   NEXT

   RETURN
