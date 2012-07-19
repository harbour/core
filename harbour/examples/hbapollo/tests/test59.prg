/*
 * $Id$
 */
/*
  sx_AppendFrom()
  DEMO APPEND FROM DBF FILE

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

   LOCAL cSource  := "test\test.dbf"
   LOCAL n := seconds()

   ?
   ? 'USE cSource'
   ? 'COPY STRUCTURE TO sixtest'
   ? 'CLOSE DATABASE'
   ? 'USE "sixtest"'
   ? 'APPEND FROM cSource'

   USE cSource
   COPY STRUCTURE TO sixtest.dbf
   CLOSE DATABASE
   USE "sixtest"
   APPEND FROM cSource
   // APPEND FROM cSource FOR STATE='IA' VIA SDENSX
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
