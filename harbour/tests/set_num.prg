/*
 * $Id$
 */

// Testing SET

#include "set.ch"

PROCEDURE Main()

   LOCAL n

   FOR n := 1 TO _SET_COUNT
      OutStd( hb_eol() )
      OutStd( Set( n ) )
   NEXT

   RETURN
