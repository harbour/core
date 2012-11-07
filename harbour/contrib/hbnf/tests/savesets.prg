/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL aSets := ft_SaveSets()

   HB_SYMBOL_UNUSED( aSets )

   Inkey( 0 )

   RETURN
