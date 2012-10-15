/*
 * $Id$
 */

#include "simpleio.ch"

PROCEDURE Main()

   ? OS(), Version()
   IF ! hb_dbExists( "_tst.dbf" )
      dbCreate( "_tst", { { "F1", "C", 1, 0 } } )
   ENDIF
   IF ! hb_dbExists( "_tst2.dbf" )
      dbCreate( "_tst2", { { "F1", "C", 1, 0 } } )
   ENDIF

   USE _tst NEW ALIAS "ONE" EXCLUSIVE
   ? Select(), Alias(), NetErr(), Used()
   ?

   mkTest( .T., "NORDD", , "TWO", .T., .F. )
   mkTest( .T., "DBF", , "TWO", .T., .F. )
   mkTest( .T., "DBF", "", "TWO", .T., .F. )
   mkTest( .T., "DBF", "nofile", "TWO", .T., .F. )
   mkTest( .T., "DBF", "_tst2", "ONE", .T., .F. )
   mkTest( .T., "DBF", "_tst", "ONE", .T., .F. )
   mkTest( .T., "DBF", "_tst", "TWO", .T., .F. )
   ?
   dbUseArea( .T., "DBF", "_tst", "ONE", .T., .F. )
   ? Select(), Alias(), NetErr(), Used()
   dbUseArea( .T., "DBF", "_tst", "TWO", .T., .F. )
   ? Select(), Alias(), NetErr(), Used()
   ?
   dbSelectArea( 1 )
   mkTest( .F., "NORDD", , "TWO", .T., .F. )
   ?

   dbCloseAll()

   hb_dbDrop( "_tst.dbf" )
   hb_dbDrop( "_tst2.dbf" )

   RETURN

PROCEDURE mkTest( lNewArea, cRdd, cFile, cAlias, lShared, lReadOnly )

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
