/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#ifdef __HARBOUR__
#require "hbct"
#endif

#include "simpleio.ch"

PROCEDURE Main()

   ? NumLine( "" ), 0
   ? NumLine( "-" ), 1
   ? NumLine( Replicate( "-", 80 ) ), 2
   ? NumLine( Replicate( "-", 160 ) ), 3
   ? NumLine( Replicate( "-", 100 ), 30 ), 4
   ? NumLine( "-" + Chr( 13 ) + Chr( 10 ) ), 2
   ? NumLine( "-" + Chr( 10 ) ), 2
   ? NumLine( "-" + Chr( 13 ) + Chr( 10 ) + "=" ), 2
   ? NumLine( "-" + Chr( 10 ) + "=" ), 2
   ? NumLine( Replicate( "-", 100 ) + Chr( 13 ) + Chr( 10 ), 30 ), 5
   ? NumLine( Replicate( "-", 100 ) + Chr( 10 ), 30 ), 5

   RETURN
