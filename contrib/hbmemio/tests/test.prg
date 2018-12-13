#require "hbmemio"

REQUEST HB_MEMIO

PROCEDURE Main()

   LOCAL tmp

   dbCreate( "mem:test", { { "F1", "N", 9, 0 } }, , .T., "memarea" )
   FOR tmp := 1 TO 1000
      dbAppend()
      FIELD->F1 := hb_Random() * 1000000
   NEXT
   INDEX ON FIELD->F1 TAG f1
   dbEval( {|| QOut( FIELD->F1 ) } )
   dbCloseArea()
   dbDrop( "mem:test" )  /* Free memory resource */

   RETURN
