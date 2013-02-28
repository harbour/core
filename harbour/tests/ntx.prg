/*
 * $Id$
 */

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL i := 0
   FIELD Last, First

   USE test
   INDEX ON Left( Last, 8 ) + Left( First, 8 ) TO test1
   INDEX ON Left( Last, 8 ) TO test2
   INDEX ON Last TO test3
   SET INDEX TO test1, test2, test3

   SET ORDER TO 1
   ? IndexKey()
   Inkey( 0 )
   GO TOP
   DO WHILE ! Eof()
      ? ++i, Last, First
      SKIP
   ENDDO

   ? "------------"
   Inkey( 0 )
   SKIP -1

   DO WHILE ! Bof()
      ? i-- , Last, First
      SKIP -1
   ENDDO

   i := 0
   SET ORDER TO 2
   ? IndexKey()
   Inkey( 0 )
   GO TOP
   DO WHILE ! Eof()
      ? ++i, Last, First
      SKIP
   ENDDO

   ? "------------"
   Inkey( 0 )
   SKIP -1

   DO WHILE ! Bof()
      ? i-- , Last, First
      SKIP -1
   ENDDO

   i := 0
   SET ORDER TO 3
   ? IndexKey()
   Inkey( 0 )
   GO TOP
   DO WHILE ! Eof()
      ? ++i, Last, First
      SKIP
   ENDDO

   ? "------------"
   Inkey( 0 )
   SKIP -1

   DO WHILE ! Bof()
      ? i-- , Last, First
      SKIP -1
   ENDDO

   USE

   hb_dbDrop( "test1.ntx" )
   hb_dbDrop( "test2.ntx" )
   hb_dbDrop( "test3.ntx" )

   RETURN
