/*
 * $Id$
 */

function main()

   local aRdd := rddList()

   QOut( "Registered RDD's:", LTrim( Str( Len( aRdd ) ) ), "=>" )
   aEval( aRdd, { | cDriver | QQOut( "", cDriver ) } )
   QOut()
   dbUseArea(,, "testdbf.dbf" )
   Bof()
   dbSelectArea( 2 )
   dbUseArea(, "SDF", "testdbf.dbf" )
   rddSetDefault("DBF")
   dbSelectArea( 3 )
   dbUseArea(,, "testdbf.dbf" )
   Eof()
   dbSelectArea( 4 )
   dbUseArea(, "DELIM", "testdbf.dbf" )
   Found()
   dbGoBottom()
   dbGoTo( 1 )
   dbSelectArea( 5 )
   dbUseArea(, "DBFNTX", "testdbf.dbf" )
   dbGoTop()
   dbSkip()
   dbCloseArea()
   dbCloseAll()

return nil

