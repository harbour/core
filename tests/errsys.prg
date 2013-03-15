/*
 * $Id$
 */

// Testing Harbour Error system

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL n

   ? "We are running and now an error will raise"

   n++      // an error should raise here

   HB_SYMBOL_UNUSED( n )

   RETURN
