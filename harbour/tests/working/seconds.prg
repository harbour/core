//
// $Id$
//

/* Test SECONDS() */
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#ifdef __HARBOUR__
 #define NEWLINE CHR(10)
#else
 #define NEWLINE CHR(13)+CHR(10)
#endif

function Main( cParam )
local n, limit := 10

   IF( ! EMPTY( cParam ) )
      limit := VAL( cParam )
   END IF
   OUTSTD( SECONDS() )
   OUTSTD( NEWLINE )
   FOR n := 1 TO limit
      IF( EMPTY( cParam ) )
         __ACCEPT( "Pause: " )
      END IF
      OUTSTD( SECONDS() )
      OUTSTD( NEWLINE )
   NEXT

RETURN NIL
