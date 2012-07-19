/*
 * $Id$
 */
/*
   Showing field properties
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL nArea, i, j
   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { { "TEST","C",10,0 }, ;
      { "MYDATE", "D", 8, 0 }, ;
      { "MYNUM", "N", 8, 2 }, ;
      { "MYMEMO", "M", 10, 0 }, ;
      { "MYLOGIC", "L", 1, 0 } }

   //IF FILE( "sixtest.nsx" )
   //   FERASE( "sixtest.nsx" )
   //ENDIF
   // SX_DISABLEAUTOOPEN()
   // SX_SETAUTOOPEN(.t.)

   CREATE DBF cFile STRUCT aStruct RDD SDENSX

   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE
   ? "sx_version()                = ", sx_version()
   ? "Alias()                     = ", Alias()
   ?
   ? "FCount()                    = ", i := FCount()
   ?

   for j := 1 TO i
      ? "FieldName(" + ltrim( str( j ) ) + ")             = ", FieldName( j )
   next

   ?
   ? 'FieldType("TEST")        = ', FieldType( "TEST" )
   ? 'FieldType("MYDATE")      = ', FieldType( "MYDATE" )
   ? 'FieldType("MYNUM")       = ', FieldType( "MYNUM" )
   ? 'FieldType("MYMEMO")      = ', FieldType( "MYMEMO" )
   ? 'FieldType("MYLOGIC")     = ', FieldType( "MYLOGIC" )
   ?
   ? 'FieldPos( TEST )         = ', FieldPos( TEST )
   ? 'FieldPos( MYDATE )       = ', FieldPos( MYDATE )
   ? 'FieldPos( MYNUM )        = ', FieldPos( MYNUM )
   ? 'FieldPos( MYMEMO )       = ', FieldPos( MYMEMO )
   ? 'FieldPos( MYLOGIC )      = ', FieldPos( MYLOGIC )
   ?
   ? 'FieldWidth("TEST")       = ', FieldWidth( "TEST" )
   ? 'FieldWidth("MYDATE")     = ', FieldWidth( "MYDATE" )
   ? 'FieldWidth("MYNUM")      = ', FieldWidth( "MYNUM" )
   ? 'FieldWidth("MYMEMO")     = ', FieldWidth( "MYMEMO" )
   ? 'FieldWidth("MYLOGIC")    = ', FieldWidth( "MYLOGIC" )
   ?
   ? 'FieldOffset("TEST")      = ', FieldOffset( "TEST" )
   ? 'FieldOffset("MYDATE")    = ', FieldOffset( "MYDATE" )
   ? 'FieldOffset("MYNUM")     = ', FieldOffset( "MYNUM" )
   ? 'FieldOffset("MYMEMO")    = ', FieldOffset( "MYMEMO" )
   ? 'FieldOffset("MYLOGIC")   = ', FieldOffset( "MYLOGIC" )
   ?
   ? 'FieldDecimals("TEST")    = ', FieldDecimals( "TEST" )
   ? 'FieldDecimals("MYDATE")  = ', FieldDecimals( "MYDATE" )
   ? 'FieldDecimals("MYNUM")   = ', FieldDecimals( "MYNUM" )
   ? 'FieldDecimals("MYMEMO")  = ', FieldDecimals( "MYMEMO" )
   ? 'FieldDecimals("MYLOGIC") = ', FieldDecimals( "MYLOGIC" )

   CLOSE DATABASE
