//
// $Id$
//

/* Test SECONDS() */
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

function Main( cParam )
local n, limit := 10
local cNewLine := OS_NewLine()

   IF( ! EMPTY( cParam ) )
      limit := VAL( cParam )
   END IF
   OUTSTD( cNewLine )
   OUTSTD( SECONDS() )
   FOR n := 1 TO limit
      IF( EMPTY( cParam ) )
         OUTSTD( cNewLine )
         OUTSTD( "Pause: " )
         INKEY(0)
      END IF
      OUTSTD( cNewLine )
      OUTSTD( SECONDS() )
   NEXT

RETURN NIL
