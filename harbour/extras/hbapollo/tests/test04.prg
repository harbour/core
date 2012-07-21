/*
 * $Id$
 */
/*
   BOF(), EOF(), SKIP, GOTOP, GOBOTTOM, GOTO, ZAP
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL nArea, i
   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { { "NAME","C",10,0 } }

/* ALIAS is assigned so the file will be automatically opened
   after created
*/

   CREATE DBF cFile STRUCT aStruct RDD SDENSX ALIAS MYALIAS

/* ALIAS is NOT assigned during creation, must open file manually
   CREATE DBF cFile STRUCT aStruct RDD SDENSX
   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE
*/

   ? "Appending 1000 records ..."
   FOR i := 1 TO 1000
      APPEND BLANK
      REPLACE NAME WITH "NAME_" + PADL( i, 5, "0" )
   NEXT
   COMMIT

   ? "Finished appending records ..."
   ? "RecCount() = ", M_Say( RecCount() )
   ? "Now go to top of file ..."
   GO TOP
   ? "RecNo() = ", M_Say( RecNo() )
   ? "Now go to bottom ..."
   GO BOTTOM
   ? "RecNo() = ", M_Say( RecNo() )

   ?
   ? "Begin skipping until EOF ... Press any Key ..."
   GO TOP
   PAUSE
   WHILE !EOF()
      ? FieldGet( NAME )
      SKIP
   ENDDO

   ?
   ? "Begin skipping backward until BOF ... Press any Key ..."
   PAUSE
   WHILE !BOF()
      ? FieldGet( NAME )
      SKIP - 1
   ENDDO

   ?
   ? "Begin skipping (skip 2) until EOF ... Press any Key ..."
   PAUSE
   WHILE !EOF()
      ? FieldGet( NAME )
      SKIP 2
   ENDDO

   ?
   ? "Now Goto 500 .. Press any key ..."
   GO 500
   PAUSE
   ?
   ? 'FieldGet( NAME ) = ', FieldGet( NAME )
   ? "RecNo()          = ", M_Say( RecNo() )

   ?
   ? "Now will ZAP Database .. Press any key ..."
   PAUSE
   ZAP
   ? "RecCount() = ", M_Say( RecCount() )
   ?
   ? "Test completed ..."

   CLOSE DATABASE

STATIC FUNCTION M_Say( nNumber )

   RETURN ltrim( str( nNumber ) )
