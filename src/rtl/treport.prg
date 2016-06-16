/*
 * HBreportForm class and __ReportForm()
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#pragma -gc0

#include "hbclass.ch"

#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"

#define F_OK            0       // No error
#define F_EMPTY         -3      // File is empty

#define _RFRM_PAGENO            3       // "Page No."
#define _RFRM_SUBTOTAL          4       // "** Subtotal **"
#define _RFRM_SUBSUBTOTAL       5       // "* Subsubtotal *"
#define _RFRM_TOTAL             6       // "*** Total ***"

#define RPT_HEADER      1       // Array of header strings
#define RPT_WIDTH       2       // Numeric, report page width
#define RPT_LMARGIN     3       // Numeric, report page offset
#define RPT_RMARGIN     4       // NIL, Not used
#define RPT_LINES       5       // Numeric, number of lines per page
#define RPT_SPACING     6       // Numeric, single=1, double=2
#define RPT_BEJECT      7       // Logical, eject before 1st page, .T.=Yes .F.=No
#define RPT_AEJECT      8       // Logical, eject after last page, .T.=Yes .F.=No
#define RPT_PLAIN       9       // Logical, plain report, .T.=Yes .F.=No
#define RPT_SUMMARY     10      // Logical, no detail lines, .T.=Yes .F.=No
#define RPT_COLUMNS     11      // Array of Column arrays
#define RPT_GROUPS      12      // Array of Group arrays
#define RPT_HEADING     13      // Character, heading for the report

#define RPT_COUNT       13      // Number of elements in the Report array


// Column array definitions ( one array per column definition )
#define RCT_EXP         1       // Block, contains compiled column expression
#define RCT_TEXT        2       // Character, contains text column expression
#define RCT_TYPE        3       // Character, type of expression
#define RCT_HEADER      4       // Array of column heading strings
#define RCT_WIDTH       5       // Numeric, column width including decimals and decimal point
#define RCT_DECIMALS    6       // Numeric, number of decimal places
#define RCT_TOTAL       7       // Logical, total this column, .T.=Yes .F.=No
#define RCT_PICT        8       // Character, picture string

#define RCT_COUNT       8       // Number of elements in the Column array


// Group array definitions ( one array per group definition )
#define RGT_EXP         1       // Block, contains compiled group expression
#define RGT_TEXT        2       // Character, contains text group expression
#define RGT_TYPE        3       // Character, type of expression
#define RGT_HEADER      4       // Character, column heading string
#define RGT_AEJECT      5       // Logical, eject after group, .T.=Yes .F.=No

#define RGT_COUNT       5       // Number of elements in the Group array

#define SIZE_FILE_BUFF          1990    // Size of report file
#define SIZE_LENGTHS_BUFF       110
#define SIZE_OFFSETS_BUFF       110
#define SIZE_EXPR_BUFF          1440
#define SIZE_FIELDS_BUFF        300
#define SIZE_PARAMS_BUFF        24

// Definitions for offsets into the FILE_BUFF string
#define LENGTHS_OFFSET          5       // Start of expression length array
#define OFFSETS_OFFSET          115     // Start of expression position array
#define EXPR_OFFSET             225     // Start of expression data area
#define FIELDS_OFFSET           1665    // Start of report columns (fields)
#define PARAMS_OFFSET           1965    // Start of report parameters block

// These are offsets into the FIELDS_BUFF string to actual values
// Values are added to a block offset FLD_OFFSET that is moved in
// increments of 12
#define FIELD_WIDTH_OFFSET      1
#define FIELD_TOTALS_OFFSET     6
#define FIELD_DECIMALS_OFFSET   7

// These are offsets into FIELDS_BUFF which are used to 'point' into
// the EXPR_BUFF string which contains the textual data
#define FIELD_CONTENT_EXPR_OFFSET       9
#define FIELD_HEADER_EXPR_OFFSET        11

// These are actual offsets into the PARAMS_BUFF string which
// are used to 'point' into the EXPR_BUFF string
#define PAGE_HDR_OFFSET         1
#define GRP_EXPR_OFFSET         3
#define SUB_EXPR_OFFSET         5
#define GRP_HDR_OFFSET          7
#define SUB_HDR_OFFSET          9

// These are actual offsets into the PARAMS_BUFF string to actual values
#define PAGE_WIDTH_OFFSET       11
#define LNS_PER_PAGE_OFFSET     13
#define LEFT_MRGN_OFFSET        15
#define RIGHT_MGRN_OFFSET       17
#define COL_COUNT_OFFSET        19
#define DBL_SPACE_OFFSET        21
#define SUMMARY_RPT_OFFSET      22
#define PE_OFFSET               23
#define OPTION_OFFSET           24

CREATE CLASS HBReportForm

   VAR aReportData    INIT {}
   VAR aReportTotals  INIT {}
   VAR aGroupTotals   INIT {}
   VAR nPageNumber
   VAR nLinesLeft
   VAR lFirstPass
   VAR lFormFeeds
   VAR nMaxLinesAvail
   VAR cExprBuff      AS STRING
   VAR cOffsetsBuff   AS STRING
   VAR cLengthsBuff   AS STRING

   METHOD New( cFrmName AS STRING, ;
      lPrinter AS LOGICAL, ;
      cAltFile AS STRING, ;
      lNoConsole AS LOGICAL, ;
      bFor AS CODEBLOCK, ;
      bWhile AS CODEBLOCK, ;
      nNext AS NUMERIC, ;
      nRecord AS NUMERIC, ;
      lRest AS LOGICAL, ;
      lPlain AS LOGICAL, ;
      cHeading AS STRING, ;
      lBEject AS LOGICAL, ;
      lSummary AS LOGICAL )

   METHOD ExecuteReport()
   METHOD ReportHeader()
   METHOD EjectPage()
   METHOD PrintIt( cString AS STRING )
   METHOD LoadReportFile( cFrmFile AS STRING )
   METHOD GetExpr( nPointer AS NUMERIC )
   METHOD GetColumn( cFieldsBuffer AS STRING, nOffset AS NUMERIC )

ENDCLASS

METHOD PROCEDURE New( cFrmName AS STRING, ;
      lPrinter AS LOGICAL, ;
      cAltFile AS STRING, ;
      lNoConsole AS LOGICAL, ;
      bFor AS CODEBLOCK, ;
      bWhile AS CODEBLOCK, ;
      nNext AS NUMERIC, ;
      nRecord AS NUMERIC, ;
      lRest AS LOGICAL, ;
      lPlain AS LOGICAL, ;
      cHeading AS STRING, ;
      lBEject AS LOGICAL, ;
      lSummary AS LOGICAL ) CLASS HBReportForm

   LOCAL lPrintOn, lConsoleOn // Status of PRINTER and CONSOLE
   LOCAL cExtraFile, lExtraState // Status of EXTRA
   LOCAL nCol, aCol, nGroup
   LOCAL xBreakVal, lBroke := .F.
   LOCAL err

   LOCAL lAnyTotals
   LOCAL lAnySubTotals

   // Resolve parameters

   IF cFRMName == NIL
      err := ErrorNew()
      err:severity := ES_ERROR
      err:genCode := EG_ARG
      err:subSystem := "FRMLBL"
      Eval( ErrorBlock(), err )
   ELSE
      /* NOTE: CA-Cl*pper does an RTrim() on the filename here,
               but in Harbour we're using _SET_TRIMFILENAME. */
      IF Set( _SET_DEFEXTENSIONS )
         cFRMName := hb_FNameExtSetDef( cFRMName, ".frm" )
      ENDIF
   ENDIF

   __defaultNIL( @cHeading, "" )

   // Set output devices

   lPrintOn   := iif( lPrinter, Set( _SET_PRINTER, lPrinter ), Set( _SET_PRINTER ) )
   lConsoleOn := iif( lNoConsole, Set( _SET_CONSOLE, .F. ), Set( _SET_CONSOLE ) )

   ::lFormFeeds := lPrinter

   IF HB_ISSTRING( cAltFile ) .AND. ! HB_ISNULL( cAltFile )   // To file
      lExtraState := Set( _SET_EXTRA, .T. )
      cExtraFile := Set( _SET_EXTRAFILE, cAltFile )
   ENDIF

   BEGIN SEQUENCE

      ::aReportData := ::LoadReportFile( cFRMName )  // Load the frm into an array
      ::nMaxLinesAvail := ::aReportData[ RPT_LINES ]

      // Modify ::aReportData based on the report parameters
      IF lSummary                    // Set the summary only flag
         ::aReportData[ RPT_SUMMARY ] := lSummary
      ENDIF
      IF lBEject != NIL .AND. lBEject
         ::aReportData[ RPT_BEJECT ]  := .F.
      ENDIF
      IF lPlain                      // Set plain report flag
         ::aReportData[ RPT_PLAIN ]   := .T.
         cHeading                     := ""
         ::lFormFeeds                 := .F.
      ENDIF
      ::aReportData[ RPT_HEADING ]    := cHeading

      // Add to the left margin if a SET MARGIN has been defined
      // NOTE: uncommenting this line will cause REPORT FORM to respect
      // SET MARGIN to screen/to file, but double the margin TO PRINT
      // ::aReportData[ RPT_LMARGIN ] += Set( _SET_MARGIN )

      ::nPageNumber := 1                  // Set the initial page number
      ::lFirstPass  := .T.                // Set the first pass flag

      ::nLinesLeft  := ::aReportData[ RPT_LINES ]

      QOut()       // output additional line on first page (S87 compatibility)
      ::nLinesLeft--

      // Check to see if a "before report" eject, or TO FILE has been specified
      IF ::aReportData[ RPT_BEJECT ]
         ::EjectPage()
      ENDIF

      // Generate the initial report header manually (in case there are no
      // records that match the report scope)
      ::ReportHeader()

      // Initialize ::aReportTotals to track both group and report totals, then
      // set the column total elements to 0 if they are to be totaled, otherwise
      // leave them NIL
      ::aReportTotals := Array( Len( ::aReportData[ RPT_GROUPS ] ) + 1, ;
         Len( ::aReportData[ RPT_COLUMNS ] ) )

      // Column total elements
      FOR nCol := 1 TO Len( ::aReportData[ RPT_COLUMNS ] )
         IF ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_TOTAL ]
            FOR nGroup := 1 TO Len( ::aReportTotals )
               ::aReportTotals[ nGroup ][ nCol ] := 0
            NEXT
         ENDIF
      NEXT

      // Initialize ::aGroupTotals as an array
      ::aGroupTotals := Array( Len( ::aReportData[ RPT_GROUPS ] ) )

      // Execute the actual report based on matching records
      dbEval( {|| ::ExecuteReport() }, bFor, bWhile, nNext, nRecord, lRest )

      // Generate any totals that may have been identified
      // Make a pass through all the groups
      FOR nGroup := Len( ::aReportData[ RPT_GROUPS ] ) TO 1 STEP -1

         // make sure group has subtotals
         lAnySubTotals := .F.
         FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
            IF aCol[ RCT_TOTAL ]
               lAnySubTotals := .T.
               EXIT              // NOTE
            ENDIF
         NEXT

         IF ! lAnySubTotals
            LOOP                 // NOTE
         ENDIF

         // Check to see if we need to eject the page
         IF ::nLinesLeft < 2
            ::EjectPage()
            IF ::aReportData[ RPT_PLAIN ]
               ::nLinesLeft := 1000
            ELSE
               ::ReportHeader()
            ENDIF
         ENDIF

         // Print the first line
         ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + ;
            __natMsg( iif( nGroup == 1, _RFRM_SUBTOTAL, _RFRM_SUBSUBTOTAL ) ) )

         // Print the second line
         QQOut( Space( ::aReportData[ RPT_LMARGIN ] ) )
         FOR nCol := 1 TO Len( ::aReportData[ RPT_COLUMNS ] )
            IF nCol > 1
               QQOut( " " )
            ENDIF
            IF ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_TOTAL ]
               QQOut( Transform( ::aReportTotals[ nGroup + 1 ][ nCol ], ;
                  ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_PICT ] ) )
            ELSE
               QQOut( Space( ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_WIDTH ] ) )
            ENDIF
         NEXT

         // Send an EOL for the last line
         QOut()
      NEXT

      // Any report totals?
      lAnyTotals := .F.
      FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
         IF aCol[ RCT_TOTAL ]
            lAnyTotals := .T.
            EXIT
         ENDIF
      NEXT

      IF lAnyTotals

         // Check to see if we need to eject the page
         IF ::nLinesLeft < 2
            ::EjectPage()
            IF ::aReportData[ RPT_PLAIN ]
               ::nLinesLeft := 1000
            ELSE
               ::ReportHeader()
            ENDIF
         ENDIF

         // Print the first line
         ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + __natMsg( _RFRM_TOTAL ) )

         // Print the second line
         QQOut( Space( ::aReportData[ RPT_LMARGIN ] ) )
         FOR nCol := 1 TO Len( ::aReportData[ RPT_COLUMNS ] )
            IF nCol > 1
               QQOut( " " )
            ENDIF
            IF ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_TOTAL ]
               QQOut( Transform( ::aReportTotals[ 1 ][ nCol ], ;
                  ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_PICT ] ) )
            ELSE
               QQOut( Space( ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_WIDTH ] ) )
            ENDIF
         NEXT

         // Send an EOL for the last line
         QOut()
      ENDIF

      // Check to see if an "after report" eject, or TO FILE has been specified
      IF ::aReportData[ RPT_AEJECT ]
         ::EjectPage()
      ENDIF

   RECOVER USING xBreakVal

      lBroke := .T.

   END SEQUENCE

   // Clean up and leave
   ::aReportData    := NIL          // Recover the space
   ::aReportTotals  := NIL
   ::aGroupTotals   := NIL
   ::nPageNumber    := NIL
   ::lFirstPass     := NIL
   ::nLinesLeft     := NIL
   ::lFormFeeds     := NIL
   ::nMaxLinesAvail := NIL

   // clean up
   Set( _SET_PRINTER, lPrintOn )    // Set the printer back to prior state
   Set( _SET_CONSOLE, lConsoleOn )  // Set the console back to prior state

   IF HB_ISSTRING( cAltFile ) .AND. ! HB_ISNULL( cAltFile )       // Set extrafile back
      Set( _SET_EXTRAFILE, cExtraFile )
      Set( _SET_EXTRA, lExtraState )
   ENDIF

   IF lBroke
      // keep the break value going
      BREAK xBreakVal
   ENDIF

   RETURN

METHOD PrintIt( cString AS STRING ) CLASS HBReportForm

   QQOut( hb_defaultValue( cString, "" ) )
   QOut()

   RETURN Self

METHOD EjectPage() CLASS HBReportForm

   IF ::lFormFeeds
      EJECT
   ENDIF

   RETURN Self

METHOD ReportHeader() CLASS HBReportForm

   LOCAL nLinesInHeader
   LOCAL aPageHeader    := {}
   LOCAL nHeadingLength := ::aReportData[ RPT_WIDTH ] - ::aReportData[ RPT_LMARGIN ] - 30
   LOCAL aCol, nLine, cLine, nMaxColLength, cHeader
   LOCAL nHeadline
   LOCAL nRPageSize
   LOCAL aTempPgHeader

   nRPageSize := ::aReportData[ RPT_WIDTH ] - ::aReportData[ RPT_RMARGIN ]

   IF ! ::aReportData[ RPT_PLAIN ]
      IF ::aReportData[ RPT_HEADING ] == ""
         AAdd( aPageHeader, __natMsg( _RFRM_PAGENO ) + Str( ::nPageNumber, 6 ) )
      ELSE
         aTempPgHeader := ParseHeader( ::aReportData[ RPT_HEADING ], ;
            Occurs( ";", ::aReportData[ RPT_HEADING ] ) + 1 )

         FOR EACH cLine IN aTempPgHeader
            nLinesInHeader := Max( XMLCOUNT( LTrim( cLine ), nHeadingLength ), 1 )

            FOR nHeadLine := 1 TO nLinesInHeader
               AAdd( aPageHeader, Space( 15 ) + ;
                  hb_UPadC( RTrim( XMEMOLINE( LTrim( cLine ), ;
                  nHeadingLength, nHeadLine ) ), nHeadingLength ) )
            NEXT
         NEXT

         aPageHeader[ 1 ] := hb_UStuff( aPageHeader[ 1 ], 1, 14, ;
            __natMsg( _RFRM_PAGENO ) + Str( ::nPageNumber, 6 ) )
      ENDIF

      AAdd( aPageHeader, DToC( Date() ) )
   ENDIF

   FOR EACH cLine IN ::aReportData[ RPT_HEADER ]

      nLinesInHeader := Max( XMLCOUNT( LTrim( cLine ) ), 1 )

      FOR nHeadLine := 1 TO nLinesInHeader

         cHeader := RTrim( XMEMOLINE( LTrim( cLine ),, nHeadLine ) )
         AAdd( aPageHeader, Space( ( nRPageSize - ::aReportData[ RPT_LMARGIN ] - ;
            hb_ULen( cHeader ) ) / 2 ) + cHeader )
      NEXT
   NEXT

   AAdd( aPageHeader, "" )  // S87 compat.

   nLinesInHeader := Len( aPageHeader )
   nMaxColLength := 0
   FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
      nMaxColLength := Max( Len( aCol[ RCT_HEADER ] ), nMaxColLength )
   NEXT
   FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
      ASize( aCol[ RCT_HEADER ], nMaxColLength )
   NEXT
   FOR nLine := 1 TO nMaxColLength
      AAdd( aPageHeader, "" )
   NEXT

   FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]    // Cycle through the columns
      FOR nLine := 1 TO nMaxColLength
         IF ! aCol:__enumIsFirst()
            aPageHeader[ nLinesInHeader + nLine ] += " "
         ENDIF
         IF aCol[ RCT_HEADER ][ nLine ] == NIL
            aPageHeader[ nLinesInHeader + nLine ] += Space( aCol[ RCT_WIDTH ] )
         ELSEIF aCol[ RCT_TYPE ] == "N"
            aPageHeader[ nLinesInHeader + nLine ] += ;
               hb_UPadL( aCol[ RCT_HEADER ][ nLine ], aCol[ RCT_WIDTH ] )
         ELSE
            aPageHeader[ nLinesInHeader + nLine ] += ;
               hb_UPadR( aCol[ RCT_HEADER ][ nLine ], aCol[ RCT_WIDTH ] )
         ENDIF
      NEXT
   NEXT

   // Insert the two blank lines between the heading and the actual data
   AAdd( aPageHeader, "" )
   AAdd( aPageHeader, "" )
   AEval( aPageHeader, {| HeaderLine | ;
      ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + HeaderLine ) } )

   // Set the page number and number of available lines
   ::nPageNumber++

   // adjust the line count to account for Summer '87 behavior
   ::nLinesLeft := ::aReportData[ RPT_LINES ] - Len( aPageHeader )
   ::nMaxLinesAvail := ::aReportData[ RPT_LINES ] - Len( aPageHeader )

   RETURN Self

METHOD PROCEDURE ExecuteReport() CLASS HBReportForm

   LOCAL aRecordHeader  := {}          // Header for the current record
   LOCAL aRecordToPrint := {}          // Current record to print
   LOCAL nCol                          // Counter for the column work
   LOCAL aCol
   LOCAL nGroup                        // Counter for the group work
   LOCAL lGroupChanged  := .F.         // Has any group changed?
   LOCAL lEjectGrp := .F.              // Group eject indicator
   LOCAL nMaxLines                     // Number of lines needed by record
   LOCAL nLine                         // Counter for each record line
   LOCAL cLine                         // Current line of text for parsing

   LOCAL lAnySubTotals

   // Add to the main column totals

   FOR nCol := 1 TO Len( ::aReportData[ RPT_COLUMNS ] )
      IF ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_TOTAL ]
         // If this column should be totaled, do it
         ::aReportTotals[ 1 ][ nCol ] += ;
            Eval( ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_EXP ] )
      ENDIF
   NEXT

   // Determine if any of the groups have changed.  If so, add the appropriate
   // line to aRecordHeader for totaling out the previous records
   IF ! ::lFirstPass                       // Don't bother first time through

      // Make a pass through all the groups
      FOR nGroup := Len( ::aReportData[ RPT_GROUPS ] ) TO 1 STEP -1

         // make sure group has subtotals
         lAnySubTotals := .F.
         FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
            IF aCol[ RCT_TOTAL ]
               lAnySubTotals := .T.
               EXIT              // NOTE
            ENDIF
         NEXT

         // retrieve group eject state from report form
         IF nGroup == 1
            lEjectGrp := ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_AEJECT ]
         ENDIF

         IF ! lAnySubTotals
            LOOP                 // NOTE
         ENDIF

         //  For subgroup processing: check if group has been changed
         IF MakeAStr( Eval( ::aReportData[ RPT_GROUPS ][ 1 ][ RGT_EXP ] ), ;
            ::aReportData[ RPT_GROUPS ][ 1 ][ RGT_TYPE ] ) != ::aGroupTotals[ 1 ]
            lGroupChanged := .T.
         ENDIF

         //  If this (sub)group has changed since the last record
         IF lGroupChanged .OR. MakeAStr( Eval( ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_EXP ] ), ;
            ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_TYPE ] ) != ::aGroupTotals[ nGroup ]

            AAdd( aRecordHeader, __natMsg( iif( nGroup == 1, _RFRM_SUBTOTAL, _RFRM_SUBSUBTOTAL ) ) )
            AAdd( aRecordHeader, "" )

            // Cycle through the columns, adding either the group
            // amount from ::aReportTotals or spaces wide enough for
            // the non-totaled columns
            FOR nCol := 1 TO Len( ::aReportData[ RPT_COLUMNS ] )
               IF ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_TOTAL ]
                  aRecordHeader[ Len( aRecordHeader ) ] += ;
                     Transform( ::aReportTotals[ nGroup + 1 ][ nCol ], ;
                     ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_PICT ] )
                  // Zero out the group totals column from aReportTotals
                  ::aReportTotals[ nGroup + 1 ][ nCol ] := 0
               ELSE
                  aRecordHeader[ Len( aRecordHeader ) ] += ;
                     Space( ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_WIDTH ] )
               ENDIF
               aRecordHeader[ Len( aRecordHeader ) ] += " "
            NEXT
            // Get rid of the extra space from the last column
            aRecordHeader[ Len( aRecordHeader ) ] := hb_StrShrink( ATail( aRecordHeader ) )  /* TOFIX: use hb_UStrShrink() */
         ENDIF
      NEXT
   ENDIF

   IF Len( aRecordHeader ) > 0 .AND. lEjectGrp .AND. lGroupChanged
      IF Len( aRecordHeader ) > ::nLinesLeft
         ::EjectPage()

         IF ::aReportData[ RPT_PLAIN ]
            ::nLinesLeft := 1000
         ELSE
            ::ReportHeader()
         ENDIF
      ENDIF

      AEval( aRecordHeader, {| HeaderLine | ;
         ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + HeaderLine ) } )

      aRecordHeader := {}

      ::EjectPage()

      IF ::aReportData[ RPT_PLAIN ]
         ::nLinesLeft := 1000
      ELSE
         ::ReportHeader()
      ENDIF
   ENDIF

   // Add to aRecordHeader in the event that the group has changed and
   // new group headers need to be generated

   // Cycle through the groups
   FOR nGroup := 1 TO Len( ::aReportData[ RPT_GROUPS ] )
      // If the group has changed
      IF !( MakeAStr( Eval( ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_EXP ] ), ;
         ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_TYPE ] ) == ::aGroupTotals[ nGroup ] )

         AAdd( aRecordHeader, "" )   // The blank line

         // page eject after group

         // put CRFF after group
         IF nGroup == 1 .AND. ! ::lFirstPass .AND. ! lAnySubTotals
            IF ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_AEJECT ]
               ::nLinesLeft := 0
            ENDIF
         ENDIF

         AAdd( aRecordHeader, iif( nGroup == 1, "** ", "* " ) + ;
            ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_HEADER ] + " " + ;
            MakeAStr( Eval( ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_EXP ] ), ;
            ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_TYPE ] ) )
      ENDIF
   NEXT

   ::lFirstPass := .F.

   // Is there anything in the record header?
   IF Len( aRecordHeader ) > 0
      // Determine if aRecordHeader will fit on the current page.  If not,
      // start a new header
      IF Len( aRecordHeader ) > ::nLinesLeft
         ::EjectPage()
         IF ::aReportData[ RPT_PLAIN ]
            ::nLinesLeft := 1000
         ELSE
            ::ReportHeader()
         ENDIF
      ENDIF

      // Send aRecordHeader to the output device, resetting nLinesLeft
      AEval( aRecordHeader, {| HeaderLine | ;
         ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + HeaderLine ) } )

      ::nLinesLeft -= Len( aRecordHeader )

      // Make sure it didn't hit the bottom margin
      IF ::nLinesLeft == 0
         ::EjectPage()
         IF ::aReportData[ RPT_PLAIN ]
            ::nLinesLeft := 1000
         ELSE
            ::ReportHeader()
         ENDIF
      ENDIF
   ENDIF

   // Add to the group totals
   FOR nCol := 1 TO Len( ::aReportData[ RPT_COLUMNS ] )
      // If this column should be totaled, do it
      IF ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_TOTAL ]
         // Cycle through the groups
         FOR nGroup := 1 TO Len( ::aReportTotals ) - 1
            ::aReportTotals[ nGroup + 1 ][ nCol ] += ;
               Eval( ::aReportData[ RPT_COLUMNS ][ nCol ][ RCT_EXP ] )
         NEXT
      ENDIF
   NEXT

   // Reset the group expressions in aGroupTotals
   FOR nGroup := 1 TO Len( ::aReportData[ RPT_GROUPS ] )
      ::aGroupTotals[ nGroup ] := MakeAStr( Eval( ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_EXP ] ), ;
         ::aReportData[ RPT_GROUPS ][ nGroup ][ RGT_TYPE ] )
   NEXT

   // Only run through the record detail if this is NOT a summary report
   IF ! ::aReportData[ RPT_SUMMARY ]
      // Determine the max number of lines needed by each expression
      nMaxLines := 1
      FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
         DO CASE
         CASE aCol[ RCT_TYPE ] $ "M"
            nMaxLines := Max( XMLCOUNT( Eval( aCol[ RCT_EXP ] ), aCol[ RCT_WIDTH ] ), nMaxLines )
         CASE aCol[ RCT_TYPE ] $ "C"
            nMaxLines := Max( XMLCOUNT( StrTran( Eval( aCol[ RCT_EXP ] ), ";", hb_eol() ), aCol[ RCT_WIDTH ] ), nMaxLines )
         ENDCASE
      NEXT

      // Size aRecordToPrint to the maximum number of lines it will need, then
      // fill it with nulls
      ASize( aRecordToPrint, nMaxLines )
      AFill( aRecordToPrint, "" )

      // Load the current record into aRecordToPrint
      FOR EACH aCol IN ::aReportData[ RPT_COLUMNS ]
         FOR nLine := 1 TO nMaxLines
            // Check to see if it's a memo or character
            IF aCol[ RCT_TYPE ] $ "CM"
               //  Load the current line of the current column into cLine
               //  with multi-lines per record ";"- method
               IF aCol[ RCT_TYPE ] $ "C"
                  cLine := XMEMOLINE( RTrim( StrTran( Eval( aCol[ RCT_EXP ] ), ;
                     ";", hb_eol() ) ), ;
                     aCol[ RCT_WIDTH ], nLine )
               ELSE
                  cLine := XMEMOLINE( RTrim( Eval( aCol[ RCT_EXP ] ) ), ;
                     aCol[ RCT_WIDTH ], nLine )
               ENDIF
               cLine := hb_UPadR( cLine, aCol[ RCT_WIDTH ] )
            ELSE
               IF nLine == 1
                  cLine := Transform( Eval( aCol[ RCT_EXP ] ), ;
                     aCol[ RCT_PICT ] )
                  cLine := hb_UPadR( cLine, aCol[ RCT_WIDTH ] )
               ELSE
                  cLine := Space( aCol[ RCT_WIDTH ] )
               ENDIF
            ENDIF
            // Add it to the existing report line
            IF ! aCol:__enumIsFirst()
               aRecordToPrint[ nLine ] += " "
            ENDIF
            aRecordToPrint[ nLine ] += cLine
         NEXT
      NEXT

      // Determine if aRecordToPrint will fit on the current page
      IF Len( aRecordToPrint ) > ::nLinesLeft
         // The record will not fit on the current page - will it fit on
         // a full page?  If not, break it up and print it.
         IF Len( aRecordToPrint ) > ::nMaxLinesAvail
            // This record is HUGE!  Break it up...
            nLine := 1
            DO WHILE nLine < Len( aRecordToPrint )
               ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + aRecordToPrint[ nLine ] )
               nLine++
               ::nLinesLeft--
               IF ::nLinesLeft == 0
                  ::EjectPage()
                  IF ::aReportData[ RPT_PLAIN ]
                     ::nLinesLeft := 1000
                  ELSE
                     ::ReportHeader()
                  ENDIF
               ENDIF
            ENDDO
         ELSE
            ::EjectPage()
            IF ::aReportData[ RPT_PLAIN ]
               ::nLinesLeft := 1000
            ELSE
               ::ReportHeader()
            ENDIF
            AEval( aRecordToPrint, {| RecordLine | ;
               ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + RecordLine ) } )
            ::nLinesLeft -= Len( aRecordToPrint )
         ENDIF
      ELSE
         // Send aRecordToPrint to the output device, resetting ::nLinesLeft
         AEval( aRecordToPrint, {| RecordLine | ;
            ::PrintIt( Space( ::aReportData[ RPT_LMARGIN ] ) + RecordLine ) } )
         ::nLinesLeft -= Len( aRecordToPrint )
      ENDIF

      // Tack on the spacing for double/triple/etc.
      IF ::aReportData[ RPT_SPACING ] > 1

         /*  Double space problem in REPORT FORM at the bottom of the page  */
         IF ::nLinesLeft >= ::aReportData[ RPT_SPACING ] - 1

            FOR nLine := 2 TO ::aReportData[ RPT_SPACING ]
               ::PrintIt()
               ::nLinesLeft--
            NEXT
         ENDIF
      ENDIF
   ENDIF    // Was this a summary report?

   RETURN

METHOD LoadReportFile( cFrmFile AS STRING ) CLASS HBReportForm

   LOCAL cFieldsBuff
   LOCAL cParamsBuff
   LOCAL nFieldOffset
   LOCAL cFileBuff := Space( SIZE_FILE_BUFF )
   LOCAL cGroupExp
   LOCAL cSubGroupExp
   LOCAL nColCount                  // Number of columns in report
   LOCAL nCount
   LOCAL hFile                      // (.frm) file handle
   LOCAL nOptionByte                // Contains option byte

   LOCAL aReport[ RPT_COUNT ]       // Create report array
   LOCAL err                        // error object

   LOCAL aHeader                    // temporary storage for report form headings
   LOCAL nHeaderIndex               // index into temporary header array

   // Initialize STATIC buffer values

   ::cLengthsBuff  := ""
   ::cOffSetsBuff  := ""
   ::cExprBuff     := ""

   // Default report values
   aReport[ RPT_HEADER ]    := {}
   aReport[ RPT_WIDTH ]     := 80
   aReport[ RPT_LMARGIN ]   := 8
   aReport[ RPT_RMARGIN ]   := 0
   aReport[ RPT_LINES ]     := 58
   aReport[ RPT_SPACING ]   := 1
   aReport[ RPT_BEJECT ]    := .T.
   aReport[ RPT_AEJECT ]    := .F.
   aReport[ RPT_PLAIN ]     := .F.
   aReport[ RPT_SUMMARY ]   := .F.
   aReport[ RPT_COLUMNS ]   := {}
   aReport[ RPT_GROUPS ]    := {}
   aReport[ RPT_HEADING ]   := ""

   // Open the report file
   IF ( hFile := hb_vfOpen( cFrmFile, HB_FO_DEFAULTS ) ) == NIL
      err := ErrorNew()
      err:severity := ES_ERROR
      err:genCode := EG_OPEN
      err:subSystem := "FRMLBL"
      err:osCode := FError()
      err:filename := cFrmFile
      Eval( ErrorBlock(), err )
   ELSE
      IF hb_vfRead( hFile, @cFileBuff, SIZE_FILE_BUFF ) > 0 .AND. FError() == 0
         // Is this a .frm type file (2 at start and end of file)
         IF Bin2W( hb_BSubStr( cFileBuff, 1, 2 ) ) == 2 .AND. ;
            Bin2W( hb_BSubStr( cFileBuff, SIZE_FILE_BUFF - 1, 2 ) ) == 2

            // Fill processing buffers
            ::cLengthsBuff := hb_BSubStr( cFileBuff, LENGTHS_OFFSET, SIZE_LENGTHS_BUFF )
            ::cOffSetsBuff := hb_BSubStr( cFileBuff, OFFSETS_OFFSET, SIZE_OFFSETS_BUFF )
            ::cExprBuff    := hb_BSubStr( cFileBuff, EXPR_OFFSET, SIZE_EXPR_BUFF )
            cFieldsBuff    := hb_BSubStr( cFileBuff, FIELDS_OFFSET, SIZE_FIELDS_BUFF )
            cParamsBuff    := hb_BSubStr( cFileBuff, PARAMS_OFFSET, SIZE_PARAMS_BUFF )

            // Process report attributes
            // Report width
            aReport[ RPT_WIDTH ]   := Bin2W( hb_BSubStr( cParamsBuff, PAGE_WIDTH_OFFSET, 2 ) )

            // Lines per page
            aReport[ RPT_LINES ]   := Bin2W( hb_BSubStr( cParamsBuff, LNS_PER_PAGE_OFFSET, 2 ) )

            // Page offset (left margin)
            aReport[ RPT_LMARGIN ] := Bin2W( hb_BSubStr( cParamsBuff, LEFT_MRGN_OFFSET, 2 ) )

            // Page right margin (not used)
            aReport[ RPT_RMARGIN ] := Bin2W( hb_BSubStr( cParamsBuff, RIGHT_MGRN_OFFSET, 2 ) )

            nColCount := Bin2W( hb_BSubStr( cParamsBuff, COL_COUNT_OFFSET, 2 ) )

            // Line spacing
            // Spacing is 1, 2, or 3
            aReport[ RPT_SPACING ] := iif( hb_BSubStr( cParamsBuff, ;
               DBL_SPACE_OFFSET, 1 ) $ "YyTt", 2, 1 )

            // Summary report flag
            aReport[ RPT_SUMMARY ] := hb_BSubStr( cParamsBuff, SUMMARY_RPT_OFFSET, 1 ) $ "YyTt"

            // Process report eject and plain attributes option byte
            nOptionByte := hb_BPeek( cParamsBuff, OPTION_OFFSET )

#ifdef HB_CLP_STRICT
            IF nOptionByte <= 8  /* Bug compatibility with CA-Cl*pper for corrupted input files */
#endif
               IF hb_bitAnd( nOptionByte, 4 ) != 0
                  aReport[ RPT_PLAIN ] := .T.          // Plain page
               ENDIF
               IF hb_bitAnd( nOptionByte, 2 ) != 0
                  aReport[ RPT_AEJECT ] := .T.         // Page eject after report
               ENDIF
               IF hb_bitAnd( nOptionByte, 1 ) != 0
                  aReport[ RPT_BEJECT ] := .F.         // Page eject before report
               ENDIF
#ifdef HB_CLP_STRICT
            ENDIF
#endif

            // Page heading, report title
            // Retrieve the header stored in the .frm file
            nHeaderIndex := 4
            aHeader := ParseHeader( ::GetExpr( Bin2W( hb_BSubStr( cParamsBuff, PAGE_HDR_OFFSET, 2 ) ) ), nHeaderIndex )

            // certain that we have retrieved all heading entries from the .frm file, we
            // now retract the empty headings
            DO WHILE nHeaderIndex > 0
               IF ! Empty( aHeader[ nHeaderIndex ] )
                  EXIT
               ENDIF
               nHeaderIndex--
            ENDDO

            aReport[ RPT_HEADER ] := iif( nHeaderIndex == 0, {}, ASize( aHeader, nHeaderIndex ) )

            // Process Groups
            // Group
            IF ! Empty( cGroupExp := ::GetExpr( Bin2W( hb_BSubStr( cParamsBuff, GRP_EXPR_OFFSET, 2 ) ) ) )

               // Add a new group array
               AAdd( aReport[ RPT_GROUPS ], Array( RGT_COUNT ) )

               // Group expression
               aReport[ RPT_GROUPS ][ 1 ][ RGT_TEXT ] := cGroupExp
               aReport[ RPT_GROUPS ][ 1 ][ RGT_EXP ] := hb_macroBlock( cGroupExp )
               IF Used()
                  aReport[ RPT_GROUPS ][ 1 ][ RGT_TYPE ] := ;
                     ValType( Eval( aReport[ RPT_GROUPS ][ 1 ][ RGT_EXP ] ) )
               ENDIF

               // Group header
               aReport[ RPT_GROUPS ][ 1 ][ RGT_HEADER ] := ;
                  ::GetExpr( Bin2W( hb_BSubStr( cParamsBuff, GRP_HDR_OFFSET, 2 ) ) )

               // Page eject after group
               aReport[ RPT_GROUPS ][ 1 ][ RGT_AEJECT ] := hb_BSubStr( cParamsBuff, ;
                  PE_OFFSET, 1 ) $ "YyTt"
            ENDIF

            // Subgroup
            IF ! Empty( cSubGroupExp := ::GetExpr( Bin2W( hb_BSubStr( cParamsBuff, SUB_EXPR_OFFSET, 2 ) ) ) )

               // Add new group array
               AAdd( aReport[ RPT_GROUPS ], Array( RGT_COUNT ) )

               // Subgroup expression
               aReport[ RPT_GROUPS ][ 2 ][ RGT_TEXT ] := cSubGroupExp
               aReport[ RPT_GROUPS ][ 2 ][ RGT_EXP ] := hb_macroBlock( cSubGroupExp )
               IF Used()
                  aReport[ RPT_GROUPS ][ 2 ][ RGT_TYPE ] := ;
                     ValType( Eval( aReport[ RPT_GROUPS ][ 2 ][ RGT_EXP ] ) )
               ENDIF

               // Subgroup header
               aReport[ RPT_GROUPS ][ 2 ][ RGT_HEADER ] := ;
                  ::GetExpr( Bin2W( hb_BSubStr( cParamsBuff, SUB_HDR_OFFSET, 2 ) ) )

               // Page eject after subgroup
               aReport[ RPT_GROUPS ][ 2 ][ RGT_AEJECT ] := .F.
            ENDIF

            // Process columns
            nFieldOffset := 12      // dBASE skips first 12 byte fields block.
            FOR nCount := 1 TO nColCount
               AAdd( aReport[ RPT_COLUMNS ], ::GetColumn( cFieldsBuff, @nFieldOffset ) )
            NEXT
         ENDIF
      ENDIF
      hb_vfClose( hFile )
   ENDIF

   RETURN aReport

/* Reads an expression from EXPR_BUFF via the OFFSETS_BUFF and returns
   a pointer to offset contained in OFFSETS_BUFF that in turn points
   to an expression located in the EXPR_BUFF string.
   Notes:
      1. The expression is empty if:
          a. Passed pointer is equal to 65535
          b. Character following character pointed to by pointer is hb_BChar( 0 ) */
METHOD GetExpr( nPointer AS NUMERIC ) CLASS HBReportForm

   LOCAL nExprOffset
   LOCAL nExprLength
   LOCAL nOffsetOffset := 0
   LOCAL cString := ""

   // Stuff for dBASE compatability.

   IF nPointer != 65535

      // Convert FILE offset to CLIPPER string offset
      nPointer++

      // Calculate offset into OFFSETS_BUFF
      IF nPointer > 1
         nOffsetOffset := ( nPointer * 2 ) - 1
      ENDIF

      nExprOffset := Bin2W( hb_BSubStr( ::cOffsetsBuff, nOffsetOffset, 2 ) )
      nExprLength := Bin2W( hb_BSubStr( ::cLengthsBuff, nOffsetOffset, 2 ) )

      // EXPR_OFFSET points to a NULL, so add one (+1) to get the string
      // and subtract one (-1) from EXPR_LENGTH for correct length

      nExprOffset++
      nExprLength--

      // Extract string
      cString := hb_BSubStr( ::cExprBuff, nExprOffset, nExprLength )

      // dBASE does this so we must do it too
      // Character following character pointed to by pointer is NULL
      IF hb_BLeft( cString, 1 ) == hb_BChar( 0 )
         cString := ""
      ENDIF
   ENDIF

   RETURN cString

STATIC FUNCTION Occurs( cSearch, cTarget )

   LOCAL nPos, nCount := 0

   DO WHILE ( nPos := hb_UAt( cSearch, cTarget ) ) > 0
      nCount++
      cTarget := hb_USubStr( cTarget, nPos + 1 )
   ENDDO

   RETURN nCount

STATIC FUNCTION XMLCOUNT( cString, nLineLength, nTabSize, lWrap )

   hb_default( @nLineLength, 79 )
   hb_default( @nTabSize, 4 )

   IF nTabSize >= nLineLength
      nTabSize := nLineLength - 1
   ENDIF

   RETURN MLCount( RTrim( cString ), nLineLength, nTabSize, lWrap )

STATIC FUNCTION XMEMOLINE( cString, nLineLength, nLineNumber, nTabSize, lWrap )

   hb_default( @nLineLength, 79 )
   hb_default( @nTabSize, 4 )

   IF nTabSize >= nLineLength
      nTabSize := nLineLength - 1
   ENDIF

   RETURN MemoLine( cString, nLineLength, nLineNumber, nTabSize, lWrap )

STATIC FUNCTION ParseHeader( cHeaderString, nFields )

   LOCAL cItem
   LOCAL nItemCount := 0
   LOCAL aPageHeader := {}
   LOCAL nHeaderLen := 254
   LOCAL nPos

   DO WHILE ++nItemCount <= nFields

      cItem := hb_ULeft( cHeaderString, nHeaderLen )

      // check for explicit delimiter
      IF ( nPos := hb_UAt( ";", cItem ) ) > 0
         // delimiter present
         AAdd( aPageHeader, hb_ULeft( cItem, nPos - 1 ) )
      ELSE
         // empty string handling for S87 and 5.0 compatibility
         AAdd( aPageHeader, iif( Empty( cItem ), "", cItem ) )
         // empty or not, we jump past the field
         nPos := nHeaderLen
      ENDIF

      cHeaderString := hb_USubStr( cHeaderString, nPos + 1 )

   ENDDO

   RETURN aPageHeader

/* Get a COLUMN element from FIELDS_BUFF string using nOffset to point to
   the current FIELDS_OFFSET block.
   Notes:
      1. The Header or Contents expressions are empty if:
         a. Passed pointer is equal to 65535
         b. Character following character pointed to by pointer is hb_BChar( 0 ) */
METHOD GetColumn( cFieldsBuffer AS STRING, /* @ */ nOffset AS NUMERIC ) CLASS HBReportForm

   LOCAL aColumn[ RCT_COUNT ]

   // Column width
   aColumn[ RCT_WIDTH ] := Bin2W( hb_BSubStr( cFieldsBuffer, nOffset + ;
      FIELD_WIDTH_OFFSET, 2 ) )

   // Total column?
   aColumn[ RCT_TOTAL ] := ;
      hb_BSubStr( cFieldsBuffer, nOffset + FIELD_TOTALS_OFFSET, 1 ) $ "YyTt"

   // Decimals width
   aColumn[ RCT_DECIMALS ] := Bin2W( hb_BSubStr( cFieldsBuffer, nOffset + ;
      FIELD_DECIMALS_OFFSET, 2 ) )

   // Offset (relative to FIELDS_OFFSET), 'point' to
   // expression area via array OFFSETS[]

   // Content expression
   aColumn[ RCT_TEXT ] := ::GetExpr( Bin2W( ;
      hb_BSubStr( cFieldsBuffer, nOffset + FIELD_CONTENT_EXPR_OFFSET, 2 ) ) )
   aColumn[ RCT_EXP ] := hb_macroBlock( aColumn[ RCT_TEXT ] )

   // Header expression
   aColumn[ RCT_HEADER ] := hb_ATokens( ::GetExpr( Bin2W( ;
      hb_BSubStr( cFieldsBuffer, nOffset + FIELD_HEADER_EXPR_OFFSET, 2 ) ) ), ";" )

   // Column picture
   // Setup picture only if a database file is open
   IF Used()
      SWITCH aColumn[ RCT_TYPE ] := ValType( Eval( aColumn[ RCT_EXP ] ) )
      CASE "C"
      CASE "M"
         aColumn[ RCT_PICT ] := Replicate( "X", aColumn[ RCT_WIDTH ] )
         EXIT
      CASE "D"
         aColumn[ RCT_PICT ] := "@D"
         EXIT
      CASE "T"
         aColumn[ RCT_PICT ] := "@T"
         EXIT
      CASE "N"
         IF aColumn[ RCT_DECIMALS ] != 0
            aColumn[ RCT_PICT ] := ;
               Replicate( "9", aColumn[ RCT_WIDTH ] - aColumn[ RCT_DECIMALS ] - 1 ) + "." + ;
               Replicate( "9", aColumn[ RCT_DECIMALS ] )
         ELSE
            aColumn[ RCT_PICT ] := Replicate( "9", aColumn[ RCT_WIDTH ] )
         ENDIF
         EXIT
      CASE "L"
         aColumn[ RCT_PICT ] := "@L" + Replicate( "X", aColumn[ RCT_WIDTH ] - 1 )
         EXIT
      ENDSWITCH
   ENDIF

   // Update offset into ?_buffer
   nOffset += 12

   RETURN aColumn

STATIC FUNCTION MakeAStr( uVar, cType )

   SWITCH Asc( cType )
   CASE Asc( "D" )
   CASE Asc( "d" ) ; RETURN DToC( uVar )
   CASE Asc( "T" )
   CASE Asc( "t" ) ; RETURN hb_TToC( uVar )
   CASE Asc( "L" )
   CASE Asc( "l" ) ; RETURN iif( uVar, "T", "F" )
   CASE Asc( "N" )
   CASE Asc( "n" ) ; RETURN Str( uVar )
   CASE Asc( "C" )
   CASE Asc( "c" )
   CASE Asc( "M" )
   CASE Asc( "m" ) ; RETURN uVar
   ENDSWITCH

   RETURN "INVALID EXPRESSION"

PROCEDURE __ReportForm( cFRMName, lPrinter, cAltFile, lNoConsole, bFor, ;
      bWhile, nNext, nRecord, lRest, lPlain, cHeading, ;
      lBEject, lSummary )

   HBReportForm():New( cFrmName, lPrinter, cAltFile, lNoConsole, bFor, bWhile, nNext, nRecord, ;
      lRest, lPlain, cHeading, lBEject, lSummary )

   RETURN
