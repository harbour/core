/*
 * $Id$
 */
/*
  sx_AppendFrom()
  DEMO APPEND FROM TEXT FILE DELIMITED WITH TAB

  Option for VIA Clause:
  SDENTX        1  CA-Clipper DBT/NTX driver
  SDEFOX        2  FoxPro FPT/IDX/CDX driver
  SDENSX        3  Six SMT/NSX driver
  SDENSX_DBT    4  CA-Clipper DBT with NSX indexes
  COMMA_DELIM  21  Comma-Delimited Text File
  SDF_FILE     22  Space-Delimited (SDF) Text File
  TAB_DELIM    23  Tab-Delimited Text File
  OEMNTX       31  Source SDENTX file translated from OEM
  OEMFOX       32  Source SDEFOX file translated from OEM
  OEMNSX       33  Source SDENSX file translated from OEM
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cSource  := "sixtest.dbf"
   LOCAL n, nHandle
   LOCAL aStruct := { { "_FIRST","C",20,0 }, { "_LAST","C",20,0 }, ;
      { "_SALARY", "N", 12, 0 } }
   LOCAL i, cPad

   SET EPOCH 1950
   SET DATE ANSI

   // Making comma delimited dummy file to append to DBF file
   ?
   ? 'Creating Text File ...'
   nHandle := FCreate( "testing.txt" )
   FOR i := 1 TO 1000
      cPad := padl( i, 5, "0" )
      FWrite( nHAndle, "First_" + cPad + CHR( 9 ) + "Last_" + cPad + CHR( 9 ) + str( i * 2 ) + CHR( 13 ) + CHR( 10 ) )
   NEXT
   FClose( nHandle )

   ?
   ? 'USE cSource'
   ? 'COPY STRUCTURE TO sixtest'
   ? 'CLOSE DATABASE'
   ? 'USE "sixtest"'
   ? 'APPEND FROM cSource'

   CREATE TABLE cSource STRUCT aStruct
   USE "sixtest"
   n := seconds()
   APPEND FROM "testing.txt" VIA "TAB_DELIM"
   ?
   ? "Start    =", n
   ? "Finished =", seconds()
   ? "Time     =", seconds() - n
   ?
   ? "BROWSE ... Press any key ..."
   PAUSE
   CLS
   BROWSE
   CLOSE ALL
   CLS
