/*
 * $Id$
 */

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

REQUEST DBFCDX

PROCEDURE Main()

   LOCAL aRdd := rddList()

   LOCAL aStruct := { ;
      { "CHARACTER", "C", 25, 0 }, ;
      { "NUMERIC",   "N",  8, 0 }, ;
      { "DOUBLE",    "N",  8, 2 }, ;
      { "DATE",      "D",  8, 0 }, ;
      { "LOGICAL",   "L",  1, 0 } }

   SET EXCLUSIVE OFF

   QOut( "Registered RDD's:", hb_ntos( Len( aRdd ) ), "=>" )
   AEval( aRdd, {| cDriver | QQOut( "", cDriver ) } )
   QOut()
   rddSetDefault( "DBFCDX" )
   dbCreate( "testdbf", aStruct, "DBFCDX" )
   dbUseArea( , , "testdbf.dbf", "ALIAS_1" )
   ? Bof()
   dbSelectArea( 2 )
   dbUseArea( , "SDF", "testdbf.dbf", "ALIAS_2" )
   dbSelectArea( 3 )
   dbUseArea( , "DELIM", "testdbf.dbf", "ALIAS_3" )
   ? Eof()
   dbSelectArea( 4 )
   dbUseArea( , "DBFNTX", "testdbf.dbf", "ALIAS_4" )
   ? Found()
   dbGoBottom()
   dbGoto( 1 )
   dbSelectArea( 5 )
   dbUseArea( , "DBF", "testdbf.dbf", "ALIAS_5" )
   dbGoTop()
   dbSkip()
   dbCloseArea()
   dbCloseAll()
   hb_dbDrop( "testdbf",, "DBFCDX" )

   RETURN
