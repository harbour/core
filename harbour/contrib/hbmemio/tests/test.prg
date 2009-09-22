/*
 * $Id$
 */

REQUEST HB_MEMIO

PROC Main()
   LOCAL nI
   FIELD F1

   DBCREATE("mem:test", {{"F1", "N", 9, 0}},, .T., "memarea")
   FOR nI := 1 TO 1000
     DBAPPEND();  F1 := HB_RANDOM() * 1000000
   NEXT
   INDEX ON F1 TAG f1
   DBEVAL({|| QOUT(F1)})
   DBCLOSEAREA()
   DBDROP("mem:test")  // Free memory resource

   RETURN
