#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL i := 0

   FIELD Last, First

   USE test.dbf READONLY
   INDEX ON Left( field->Last, 8 ) + Left( field->First, 8 ) TO test1.ntx
   INDEX ON Left( field->Last, 8 ) TO test2.ntx
   INDEX ON field->Last TO test3.ntx
   SET INDEX TO test1.ntx, test2.ntx, test3.ntx

   ordSetFocus( 1 )
   ? IndexKey()
   Inkey( 0 )
   dbGoTop()
   DO WHILE ! Eof()
      ? ++i, Last, First
      dbSkip()
   ENDDO

   ? "------------"
   Inkey( 0 )
   dbSkip( -1 )

   DO WHILE ! Bof()
      ? i--, Last, First
      dbSkip( -1 )
   ENDDO

   i := 0
   ordSetFocus( 2 )
   ? IndexKey()
   Inkey( 0 )
   dbGoTop()
   DO WHILE ! Eof()
      ? ++i, Last, First
      dbSkip()
   ENDDO

   ? "------------"
   Inkey( 0 )
   dbSkip( -1 )

   DO WHILE ! Bof()
      ? i--, Last, First
      dbSkip( -1 )
   ENDDO

   i := 0
   ordSetFocus( 3 )
   ? IndexKey()
   Inkey( 0 )
   dbGoTop()
   DO WHILE ! Eof()
      ? ++i, Last, First
      dbSkip()
   ENDDO

   ? "------------"
   Inkey( 0 )
   dbSkip( -1 )

   DO WHILE ! Bof()
      ? i--, Last, First
      dbSkip( -1 )
   ENDDO

   dbCloseArea()

   hb_dbDrop( "test1.ntx" )
   hb_dbDrop( "test2.ntx" )
   hb_dbDrop( "test3.ntx" )

   RETURN
