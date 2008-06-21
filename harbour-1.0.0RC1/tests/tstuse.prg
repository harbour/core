/*
 * $Id$
 */

#define EOL chr(13)+chr(10)
#command ? [<x,...>] => outstd(EOL)[;outstd(<x>)]
proc main()
   ? OS(), VERSION()
   if !file("_tst.dbf")
      dbCreate("_tst",{{"F1","C",1,0}})
   endif
   if !file("_tst2.dbf")
      dbCreate("_tst2",{{"F1","C",1,0}})
   endif

   USE _tst NEW ALIAS "ONE" EXCLUSIVE
   ? select(), alias(), netErr(), used()
   ?

   mkTest( .T., "NORDD",, "TWO", .T., .F. )
   mkTest( .T., "DBF",, "TWO", .T., .F. )
   mkTest( .T., "DBF", "", "TWO", .T., .F. )
   mkTest( .T., "DBF", "nofile", "TWO", .T., .F. )
   mkTest( .T., "DBF", "_tst2", "ONE", .T., .F. )
   mkTest( .T., "DBF", "_tst", "ONE", .T., .F. )
   mkTest( .T., "DBF", "_tst", "TWO", .T., .F. )
   ?
   dbUseArea( .T., "DBF", "_tst", "ONE", .T., .F. )
   ? select(), alias(), netErr(), used()
   dbUseArea( .T., "DBF", "_tst", "TWO", .T., .F. )
   ? select(), alias(), netErr(), used()
   ?
   dbSelectArea( 1 )
   mkTest( .F., "NORDD",, "TWO", .T., .F. )
   ?
return

proc mkTest( lNewArea, cRdd, cFile, cAlias, lShared, lReadOnly )
local cbErr:=errorBlock({|oErr|break(oErr)}), oErr
netErr(.f.)
begin sequence
   dbUseArea( lNewArea, cRdd, cFile, cAlias, lShared, lReadOnly )
recover using oErr
   ? "Error:", oErr:subCode, oErr:description, oErr:operation, oErr:osCode
end
? select(), alias(), netErr(), used()
errorBlock(cbErr)
return
