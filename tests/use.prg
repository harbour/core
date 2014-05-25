#include "simpleio.ch"

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   hb_langSelect( "en" )

   ? OS(), Version()
   IF ! hb_dbExists( "_tst.dbf" )
      dbCreate( "_tst.dbf", { { "F1", "C", 1, 0 } } )
   ENDIF
   IF ! hb_dbExists( "_tst2.dbf" )
      dbCreate( "_tst2.dbf", { { "F1", "C", 1, 0 } } )
   ENDIF

   USE _tst.dbf EXCLUSIVE ALIAS "ONE"
   ? Select(), Alias(), NetErr(), Used()
   ?

   mkTest( .T., "NORDD", , "TWO", .T., .F. )
   mkTest( .T., "DBF", , "TWO", .T., .F. )
   mkTest( .T., "DBF", "", "TWO", .T., .F. )
   mkTest( .T., "DBF", "nofile", "TWO", .T., .F. )
   mkTest( .T., "DBF", "_tst2.dbf", "ONE", .T., .F. )
   mkTest( .T., "DBF", "_tst.dbf", "ONE", .T., .F. )
   mkTest( .T., "DBF", "_tst.dbf", "TWO", .T., .F. )
   ?
   dbUseArea( .T., "DBF", "_tst.dbf", "ONE", .T., .F. )
   ? Select(), Alias(), NetErr(), Used()
   dbUseArea( .T., "DBF", "_tst.dbf", "TWO", .T., .F. )
   ? Select(), Alias(), NetErr(), Used()
   ?
   dbSelectArea( 1 )
   mkTest( .F., "NORDD", , "TWO", .T., .F. )
   ?

   dbCloseAll()

   hb_dbDrop( "_tst.dbf" )
   hb_dbDrop( "_tst2.dbf" )

   RETURN

STATIC PROCEDURE mkTest( lNewArea, cRdd, cFile, cAlias, lShared, lReadOnly )

   LOCAL cbErr := ErrorBlock( {| oErr | Break( oErr ) } ), oErr

   NetErr( .F. )
   BEGIN SEQUENCE
      dbUseArea( lNewArea, cRdd, cFile, cAlias, lShared, lReadOnly )
   RECOVER USING oErr
      ? "Error:", oErr:subCode, oErr:description, oErr:operation, oErr:osCode
   END SEQUENCE
   ? Select(), Alias(), NetErr(), Used()
   ErrorBlock( cbErr )

   RETURN
