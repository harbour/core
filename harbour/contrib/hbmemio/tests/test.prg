/*
 * $Id$
 */

#require "hbmemio"

REQUEST HB_MEMIO

PROCEDURE Main()

   LOCAL nI
   FIELD F1

   dbCreate( "mem:test", { { "F1", "N", 9, 0 } }, , .T., "memarea" )
   FOR nI := 1 TO 1000
      dbAppend();  F1 := hb_Random() * 1000000
   NEXT
   INDEX ON F1 TAG f1
   dbEval( {|| QOut( F1 ) } )
   dbCloseArea()
   dbDrop( "mem:test" )  // Free memory resource

   RETURN
