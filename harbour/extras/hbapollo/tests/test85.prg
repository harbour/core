/*
 * $Id$
 */
/*
   sx_KeyAdd(), sx_KeyDrop(), sx_KeyData()
*/
#include "sixapi.ch"

#define IDX_NONE   0  // Standard index (Not UNIQUE or EMPTY (RYO))
#define IDX_UNIQUE 1  // UNIQUE, allows unique keys only
#define IDX_EMPTY  2  // Roll-Your-Own (RYO) empty index header

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cState

   SET RDD SDEFOX
   USE "TEST/TEST" EXCLUSIVE
   ? 'Creating index ...'
   FErase( "TEST/TEST.CDX" )
   sx_IndexTag ( "TEST/TEST", "STATE", "STATE", IDX_EMPTY )
   sx_IndexTag ( "TEST/TEST", "FIRST", "FIRST" )
   sx_Gotop()
   ? 'Adding Key ...'
   WHILE !sx_Eof()
      cState := SX_GETVALUE( "STATE" )
      IF cState == "LA"
         ? 'SX_KEYADD( "STATE" ) =', SX_KEYADD( "STATE" )
      ENDIF
      sx_skip( 1 )
   ENDDO
   ? 'Now examining with sx_KeyData() .. Press any key ...'
   PAUSE
   sx_SetOrder( "STATE" )
   sx_Gotop()
   WHILE !sx_Eof()
      ? "sx_RecNo() =", ltrim( str( sx_recNo() ) ), ' sx_KeyData()=', sx_KeyData()
      sx_skip()
   ENDDO

   ? 'BROWSE .. Press any key ...'
   PAUSE
   sx_SetOrder( "STATE" )
   BROWSE
   CLS
   ? 'Now Deleting Key ....'
   sx_SetOrder( "FIRST" )
   sx_GoTop()
   WHILE !sx_Eof()
      cState := SX_GETVALUE( "STATE" )
      IF cState == "LA"
         ? 'SX_KEYDROP( "STATE" ) =', SX_KEYDROP( "STATE" )
      ENDIF
      sx_skip()
   ENDDO

   ? 'BROWSE .. Press any key ...'
   PAUSE
   sx_SetOrder( "STATE" )
   BROWSE
