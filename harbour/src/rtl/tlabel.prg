/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBLabelForm class and __LabelForm()
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "hbclass.ch"

#include "common.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"

#define F_OK            0       // No error
#define F_EMPTY         -3      // File is empty

#define _LF_SAMPLES     2       // "Do you want more samples?"
#define _LF_YN          12      // "Y/N"

#define LBL_REMARK      1       // Character, remark from label file
#define LBL_HEIGHT      2       // Numeric, label height
#define LBL_WIDTH       3       // Numeric, label width
#define LBL_LMARGIN     4       // Numeric, left margin
#define LBL_LINES       5       // Numeric, lines between labels
#define LBL_SPACES      6       // Numeric, spaces between labels
#define LBL_ACROSS      7       // Numeric, number of labels across
#define LBL_FIELDS      8       // Array of Field arrays

#define LBL_COUNT       8       // Numeric, number of label fields

// Field array definitions ( one array per field )
#define LF_EXP          1       // Block, field expression
#define LF_TEXT         2       // Character, text of field expression
#define LF_BLANK        3       // Logical, compress blank fields, .T.=Yes .F.=No

#define LF_COUNT        3       // Numeric, number of elements in field array

#define BUFFSIZE        1034    // Size of label file
#define FILEOFFSET      74      // Start of label content descriptions
#define FIELDSIZE       60
#define REMARKOFFSET    2
#define REMARKSIZE      60
#define HEIGHTOFFSET    62
#define HEIGHTSIZE      2
#define WIDTHOFFSET     64
#define WIDTHSIZE       2
#define LMARGINOFFSET   66
#define LMARGINSIZE     2
#define LINESOFFSET     68
#define LINESSIZE       2
#define SPACESOFFSET    70
#define SPACESSIZE      2
#define ACROSSOFFSET    72
#define ACROSSSIZE      2

CREATE CLASS HBLabelForm

   VAR aLabelData   AS ARRAY   INIT {}
   VAR aBandToPrint AS ARRAY
   VAR cBlank       AS STRING  INIT ""
   VAR lOneMoreBand AS LOGICAL INIT .T.
   VAR nCurrentCol  AS NUMERIC // The current column in the band

   METHOD New( cLBLName, lPrinter, cAltFile, lNoConsole, bFor, ;
                          bWhile, nNext, nRecord, lRest, lSample )

   METHOD ExecuteLabel()
   METHOD SampleLabels()
   METHOD LoadLabel( cLblFile )

ENDCLASS

METHOD New( cLBLName, lPrinter, cAltFile, lNoConsole, bFor, ;
                       bWhile, nNext, nRecord, lRest, lSample ) CLASS HBLabelForm

   LOCAL lPrintOn := .F.               // PRINTER status
   LOCAL lConsoleOn                    // CONSOLE status
   LOCAL cExtraFile, lExtraState       // EXTRA file status
   LOCAL xBreakVal, lBroke := .F.
   LOCAL err
   LOCAL OldMargin
   LOCAL cExt

   ::aBandToPrint := {} // Array(5)
   ::nCurrentCol := 1

   // Resolve parameters
   IF cLBLName == NIL
      err := ErrorNew()
      err:severity := ES_ERROR
      err:genCode := EG_ARG
      err:subSystem := "FRMLBL"
      Eval(ErrorBlock(), err)
   ELSE
      /* NOTE: CA-Cl*pper does an RTrim() on the filename here,
               but in Harbour we're using _SET_TRIMFILENAME. [vszakats] */
      IF Set( _SET_DEFEXTENSIONS )
         hb_FNameSplit( cLBLName, NIL, NIL, @cExt )
         IF Empty( cExt )
            cLBLName += ".lbl"
         ENDIF
      ENDIF
   ENDIF

   DEFAULT lPrinter TO .F.
   DEFAULT lSample TO .F.

   // Set output devices
   IF lPrinter             // To the printer
      lPrintOn  := Set( _SET_PRINTER, lPrinter )
   ENDIF

   lConsoleOn := Set( _SET_CONSOLE )
   Set( _SET_CONSOLE, ! ( lNoConsole .OR. !lConsoleOn ) )

   IF !Empty(cAltFile)         // To file
      lExtraState := Set( _SET_EXTRA, .T. )
      cExtraFile  := Set( _SET_EXTRAFILE, cAltFile )
   ENDIF

   OldMargin := Set( _SET_MARGIN, 0)

   BEGIN SEQUENCE

      ::aLabelData := ::LoadLabel( cLBLName )  // Load the (.lbl) into an array

      // Add to the left margin if a SET MARGIN has been defined
      ::aLabelData[ LBL_LMARGIN ] := ::aLabelData[ LBL_LMARGIN ] + OldMargin

      ASize( ::aBandToPrint, Len( ::aLabelData[ LBL_FIELDS ]))
      AFill( ::aBandToPrint, Space( ::aLabelData[ LBL_LMARGIN ] ) )

      // Create enough space for a blank record
      ::cBlank := Space( ::aLabelData[ LBL_WIDTH ] + ::aLabelData[ LBL_SPACES ] )

      // Handle sample labels
      IF lSample
         ::SampleLabels()
      ENDIF

      // Execute the actual label run based on matching records
      DBEval( {|| ::ExecuteLabel() }, bFor, bWhile, nNext, nRecord, lRest )

      // Print the last band if there is one
      IF ::lOneMoreBand
         // Print the band
         AEval( ::aBandToPrint, { | BandLine | PrintIt( BandLine ) } )
      ENDIF

   RECOVER USING xBreakVal

      lBroke := .T.

   END SEQUENCE

   // Clean up and leave
   ::aLabelData   := {}                // Recover the space
   ::aBandToPrint := {}
   ::nCurrentCol  := 1
   ::cBlank       := ""
   ::lOneMoreBand :=.T.

   // clean up
   Set( _SET_PRINTER, lPrintOn ) // Set the printer back to prior state
   Set( _SET_CONSOLE, lConsoleOn )  // Set the console back to prior state

   IF !Empty(cAltFile)            // Set extrafile back
      Set( _SET_EXTRAFILE, cExtraFile )
      Set( _SET_EXTRA, lExtraState )
   ENDIF

   IF lBroke
      BREAK xBreakVal               // continue breaking
   ENDIF

   Set( _SET_MARGIN, OldMargin )

   RETURN Self

METHOD ExecuteLabel() CLASS HBLabelForm
   LOCAL nField, nMoreLines, aBuffer := {}, cBuffer
   LOCAL v

   // Load the current record into aBuffer
   FOR nField := 1 TO Len( ::aLabelData[ LBL_FIELDS ] )

      if ::aLabelData[ LBL_FIELDS, nField ] != NIL

         v := Eval( ::aLabelData[ LBL_FIELDS, nField, LF_EXP ] )

         cBuffer := PadR( v, ::aLabelData[ LBL_WIDTH ] )
         cBuffer := cBuffer + Space( ::aLabelData[ LBL_SPACES ] )

         if ( ::aLabelData[ LBL_FIELDS, nField, LF_BLANK ] )
            if ( !Empty( cBuffer ) )
               AAdd( aBuffer, cBuffer )
            endif
         else
            AAdd( aBuffer, cBuffer )
         endif

      else

         AAdd( aBuffer, NIL )

      endif

   NEXT

   ASize( aBuffer, Len( ::aLabelData[ LBL_FIELDS ] ) )

   // Add aBuffer to ::aBandToPrint
   FOR nField := 1 TO Len( ::aLabelData[ LBL_FIELDS ] )
      IF aBuffer[ nField ] == NIL
         ::aBandToPrint[ nField ] := ::aBandToPrint[ nField ] + ::cBlank
      ELSE
         ::aBandToPrint[ nField ] := ::aBandToPrint[ nField ]  + aBuffer[ nField ]
      ENDIF
   NEXT

   IF ::nCurrentCol == ::aLabelData[ LBL_ACROSS ]

      // trim
      FOR nField := 1 TO Len( ::aBandToPrint )
         ::aBandToPrint[ nField ] := RTrim( ::aBandToPrint[ nField ] )
      NEXT


      ::lOneMoreBand := .F.
      ::nCurrentCol  := 1

      // Print the band
      AEval( ::aBandToPrint, { | BandLine | PrintIt( BandLine ) } )

      nMoreLines := ::aLabelData[ LBL_HEIGHT ] - Len( ::aBandToPrint )
      IF nMoreLines > 0
         FOR nField := 1 TO nMoreLines
            PrintIt()
         NEXT
      ENDIF
      IF ::aLabelData[ LBL_LINES ] > 0

         // Add the spaces between the label lines
         FOR nField := 1 TO ::aLabelData[ LBL_LINES ]
            PrintIt()
         NEXT

      ENDIF

      // Clear out the band
      AFill( ::aBandToPrint, Space( ::aLabelData[ LBL_LMARGIN ] ) )
   ELSE
      ::lOneMoreBand := .T.
      ::nCurrentCol :=  ::nCurrentCol + 1
   ENDIF

   RETURN Self

METHOD SampleLabels() CLASS HBLabelForm
   LOCAL cKey, lMoreSamples := .T., nField
   LOCAL aBand := {}

   // Create the sample label row
   ASize( aBand, ::aLabelData[ LBL_HEIGHT ] )
   AFill( aBand, Space( ::aLabelData[ LBL_LMARGIN ] ) +;
              Replicate( Replicate( "*", ;
              ::aLabelData[ LBL_WIDTH ] ) + ;
              Space( ::aLabelData[ LBL_SPACES ] ), ;
              ::aLabelData[ LBL_ACROSS ] ) )

   // Prints sample labels
   DO WHILE lMoreSamples

      // Print the samples
      AEval( aBand, { | BandLine | PrintIt( BandLine ) } )

      IF ::aLabelData[ LBL_LINES ] > 0
         // Add the spaces between the label lines
         FOR nField := 1 TO ::aLabelData[ LBL_LINES ]
            PrintIt()
         NEXT nField
      ENDIF

      // Prompt for more
      @ Row(), 0 SAY __NatMsg( _LF_SAMPLES ) + " (" + __NatMsg( _LF_YN ) + ")"
      cKey := hb_KeyChar( Inkey( 0 ) )
      @ Row(), Col() SAY cKey
      IF Row() == MaxRow()
         hb_scroll( 0, 0, MaxRow(), MaxCol(), 1 )
         @ MaxRow(), 0 SAY ""
      ELSE
         @ Row() + 1, 0 SAY ""
      ENDIF
      IF __NatIsNegative( cKey )    // Don't give sample labels
         lMoreSamples := .F.
      ENDIF
   ENDDO

   RETURN Self

METHOD LoadLabel( cLblFile ) CLASS HBLabelForm
   LOCAL i                                // Counters
   LOCAL cBuff      := Space( BUFFSIZE )  // File buffer
   LOCAL nHandle                          // File handle
   LOCAL nReadCount                       // Bytes read from file
   LOCAL nOffset    := FILEOFFSET         // Offset into file
   LOCAL nFileError                       // File error
   LOCAL cFieldText                       // Text expression container
   LOCAL err                              // error object

   LOCAL cDefPath          // contents of SET DEFAULT string
   LOCAL aPaths            // array of paths
   LOCAL nPathIndex        // iteration counter

   // Create and initialize default label array
   LOCAL aLabel[ LBL_COUNT ]
   aLabel[ LBL_REMARK ]  := Space( 60 )    // Label remark
   aLabel[ LBL_HEIGHT ]  := 5              // Label height
   aLabel[ LBL_WIDTH ]   := 35             // Label width
   aLabel[ LBL_LMARGIN ] := 0              // Left margin
   aLabel[ LBL_LINES ]   := 1              // Lines between labels
   aLabel[ LBL_SPACES ]  := 0              // Spaces between labels
   aLabel[ LBL_ACROSS ]  := 1              // Number of labels across
   aLabel[ LBL_FIELDS ]  := {}             // Array of label fields

   // Open the label file
   nHandle := FOpen( cLblFile )

   IF ! Empty( nFileError := FError() ) .AND. !( "\" $ cLblFile .OR. ":" $ cLblFile )

      // Search through default path; attempt to open label file
      cDefPath := Set( _SET_DEFAULT )
      cDefPath := StrTran( cDefPath, ",", ";" )
      aPaths := ListAsArray( cDefPath, ";" )

      FOR nPathIndex := 1 TO Len( aPaths )
         nHandle := FOpen( aPaths[ nPathIndex ] + "\" + cLblFile )
         // if no error is reported, we have our label file
         IF Empty( nFileError := FError() )
            EXIT

         ENDIF

      NEXT nPathIndex

   ENDIF

   // File error
   IF nFileError != F_OK
      err := ErrorNew()
      err:severity := ES_ERROR
      err:genCode := EG_OPEN
      err:subSystem := "FRMLBL"
      err:osCode := nFileError
      err:filename := cLblFile
      Eval(ErrorBlock(), err)
   ENDIF

   // If we got this far, assume the label file is open and ready to go
   // and so go ahead and read it
   nReadCount := FRead( nHandle, @cBuff, BUFFSIZE )

   // READ ok?
   IF nReadCount == 0
      nFileError := F_EMPTY             // File is empty
   ELSE
      nFileError := FError()            // Check for DOS errors
   ENDIF

   IF nFileError == 0

      // Load label dimension into aLabel
      aLabel[ LBL_REMARK ] := SubStr(cBuff, REMARKOFFSET, REMARKSIZE)
      aLabel[ LBL_HEIGHT ] := BIN2W(SubStr(cBuff, HEIGHTOFFSET, HEIGHTSIZE))
      aLabel[ LBL_WIDTH  ] := BIN2W(SubStr(cBuff, WIDTHOFFSET, WIDTHSIZE))
      aLabel[ LBL_LMARGIN] := BIN2W(SubStr(cBuff, LMARGINOFFSET, LMARGINSIZE))
      aLabel[ LBL_LINES  ] := BIN2W(SubStr(cBuff, LINESOFFSET, LINESSIZE))
      aLabel[ LBL_SPACES ] := BIN2W(SubStr(cBuff, SPACESOFFSET, SPACESSIZE))
      aLabel[ LBL_ACROSS ] := BIN2W(SubStr(cBuff, ACROSSOFFSET, ACROSSSIZE))

      FOR i := 1 TO aLabel[ LBL_HEIGHT ]

         // Get the text of the expression
         cFieldText := RTrim( SubStr( cBuff, nOffset, FIELDSIZE ) )
         nOffset :=nOffSet + 60

         IF !Empty( cFieldText )

            AAdd( aLabel[ LBL_FIELDS ], {} )

            // Field expression
            AAdd( aLabel[ LBL_FIELDS, i ], hb_macroBlock( cFieldText ) )

            // Text of field
            AAdd( aLabel[ LBL_FIELDS, i ], cFieldText )

            // Compression option
            AAdd( aLabel[ LBL_FIELDS, i ], .T. )

         ELSE

            AAdd( aLabel[ LBL_FIELDS ], NIL )

         ENDIF

      NEXT

      // Close file
      FClose( nHandle )

   ENDIF

   RETURN aLabel

FUNCTION __LabelForm( cLBLName, lPrinter, cAltFile, lNoConsole, bFor, ;
                       bWhile, nNext, nRecord, lRest, lSample )

   RETURN HBLabelForm():New( cLBLName, lPrinter, cAltFile, lNoConsole, bFor, ;
                          bWhile, nNext, nRecord, lRest, lSample )

STATIC PROCEDURE PrintIt( cString )

   hb_default( @cString, "" )

   QQOut( cString )
   QOut()

   RETURN

STATIC FUNCTION ListAsArray( cList, cDelimiter )

   LOCAL nPos
   LOCAL aList := {}                  // Define an empty array
   LOCAL lDelimLast := .F.

   hb_default( @cDelimiter, "," )

   DO WHILE Len( cList ) != 0

      nPos := At( cDelimiter, cList )

      IF nPos == 0
         nPos := Len( cList )
      ENDIF

      IF SubStr( cList, nPos, 1 ) == cDelimiter
         lDelimLast := .T.
         AAdd( aList, SubStr( cList, 1, nPos - 1 ) ) // Add a new element
      ELSE
         lDelimLast := .F.
         AAdd( aList, SubStr( cList, 1, nPos ) ) // Add a new element
      ENDIF

      cList := SubStr( cList, nPos + 1 )

   ENDDO

   IF lDelimLast
      AAdd( aList, "" )
   ENDIF

   RETURN aList                       // Return the array
