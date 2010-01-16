/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Printing subsystem for Windows using GUI printing
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
 * www - http://www.harbour-project.org
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

/*
  WIN_PRN() was designed to make it easy to emulate Clipper Dot Matrix printing.
  Dot Matrix printing was in CPI ( Characters per inch & Lines per inch ).
  Even though "Mapping Mode" for WIN_PRN() is MM_TEXT, ::SetFont() accepts the
  nWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
  6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
  If you do not like this then inherit from the class and override anything you want

  Simple example

  TODO: Colour printing
        etc....

  Peter Rees 21 January 2004 <peter@rees.co.nz>
*/

#include "hbclass.ch"
#include "common.ch"

#include "hbwin.ch"

CREATE CLASS WIN_PRN

   METHOD New( cPrinter )
   METHOD Create()                  // CreatesDC and sets "Courier New" font, set Orientation, Copies, Bin#
                                    // Create() ( & StartDoc() ) must be called before printing can start.
   METHOD Destroy()                 // Calls EndDoc() - restores default font, Deletes DC.
   DESTRUCTOR Destruct()

   METHOD StartDoc( cDocName )      // Calls StartPage()
   METHOD EndDoc( lAbortDoc )       // Calls EndPage() if lAbortDoc not .T.
   METHOD StartPage()
   METHOD EndPage( lStartNewPage )  // If lStartNewPage == .T. then StartPage() is called for the next page of output
   METHOD NewLine()
   METHOD NewPage( lDelay )         // If lDelay == .T. then new page is not created immediately but just before 1-st output
   METHOD CheckPage()
   METHOD GetDocumentProperties()
   METHOD SetFont( cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet )
                                                                 // NB: nWidth is in "CharactersPerInch"
                                                                 //     _OR_ { nMul, nDiv } which equates to "CharactersPerInch"
                                                                 //     _OR_ ZERO ( 0 ) which uses the default width of the font
                                                                 //          for the nPointSize
                                                                 //   IF nWidth (or nDiv) is < 0 then Fixed font is emulated

   METHOD SetDefaultFont()

   METHOD GetFonts()                                   // Returns array of { "FontName", lFixed, lTrueType, nCharSetRequired }
   METHOD Bold( nWeight )
   METHOD UnderLine( lUnderline )
   METHOD Italic( lItalic )
   METHOD SetDuplexType( nDuplexType )                 // Get/Set current Duplexmode
   METHOD SetPrintQuality( nPrintQuality )             // Get/Set Printquality
   METHOD CharSet( nCharSet )


   METHOD SetPos( nPosX, nPosY )                       // **WARNING** : ( Col, Row ) _NOT_ ( Row, Col )
   METHOD SetColor( nClrText, nClrPane, nAlign )
   METHOD SetBkMode( nMode )                                         // OPAQUE == 2 or TRANSPARENT == 1
                                                                     // Set Background mode

   METHOD TextOut( cString, lNewLine, lUpdatePosX, nAlign )     // nAlign : TA_LEFT, TA_RGIHT, TA_CENTER, TA_TOP, TA_BOTTOM, TA_BASELINE
   METHOD TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign ) // **WARNING** : ( Col, Row ) _NOT_ ( Row, Col )


   METHOD SetPen( nStyle, nWidth, nColor )
   METHOD Line( nX1, nY1, nX2, nY2 )
   METHOD Box( nX1, nY1, nX2, nY2, nWidth, nHeight )
   METHOD Arc( nX1, nY1, nX2, nY2 )
   METHOD Ellipse( nX1, nY1, nX2, nY2 )
   METHOD FillRect( nX1, nY1, nX2, nY2, nColor )
   METHOD GetCharWidth()
   METHOD GetCharHeight()
   METHOD GetTextWidth( cString )
   METHOD GetTextHeight( cString )
   METHOD DrawBitMap( oBmp )

   /* Clipper DOS compatible functions. */
   METHOD SetPrc( nRow, nCol )      // Based on ::LineHeight and current ::CharWidth
   METHOD PRow()
   METHOD PCol()
   METHOD MaxRow()                  // Based on ::LineHeight & Form dimensions
   METHOD MaxCol()                  // Based on ::CharWidth & Form dimensions

   METHOD MM_TO_POSX( nMm )      // Convert position on page from MM to pixel location Column
   METHOD MM_TO_POSY( nMm )      //   "       "      "    "    "   "  "   "      "     Row
   METHOD INCH_TO_POSX( nInch )  // Convert position on page from INCH to pixel location Column
   METHOD INCH_TO_POSY( nInch )  //   "       "      "    "    "   "    "   "       "    Row

   METHOD TextAtFont( nPosX, nPosY, cString, cFont, nPointSize,;     // Print text string at location
                      nWidth, nBold, lUnderLine, lItalic, nCharSet,; // in specified font and color.
                      lNewLine, lUpdatePosX, nColor, nAlign )        // Restore original font and colour
                                                                     // after printing.

   METHOD GetDeviceCaps( nCaps )

   VAR PrinterName      INIT ""
   VAR Printing         INIT .F.
   VAR HavePrinted      INIT .F.
   VAR PageInit         INIT .F.
   VAR PageNumber       INIT 0
   VAR hPrinterDc       INIT 0

// These next 4 variables must be set before calling ::Create() if
// you wish to alter the defaults
   VAR FormType         INIT 0
   VAR BinNumber        INIT 0
   VAR Landscape        INIT .F.
   VAR Copies           INIT 1

   VAR SetFontOk        INIT .F.
   VAR hFont            INIT 0
   VAR FontName         INIT ""                       // Current Point size for font
   VAR FontPointSize    INIT 12                       // Point size for font
   VAR FontWidth        INIT { 0, 0 }                 // {Mul, Div} Calc width: nWidth:= MulDiv(nMul, GetDeviceCaps(shDC,LOGPIXELSX), nDiv)
                                                      // If font width is specified it is in "characters per inch" to emulate DotMatrix
   VAR fBold            INIT 0      HIDDEN            // font darkness weight ( Bold). See wingdi.h or WIN SDK CreateFont() for valid values
   VAR fUnderLine       INIT .F.    HIDDEN            // UnderLine is on or off
   VAR fItalic          INIT .F.    HIDDEN            // Italic is on or off
   VAR fCharSet         INIT 1      HIDDEN            // Default character set == DEFAULT_CHARSET ( see wingdi.h )

   VAR PixelsPerInchY   INIT 0
   VAR PixelsPerInchX   INIT 0
   VAR PageHeight       INIT 0
   VAR PageWidth        INIT 0
   VAR TopMargin        INIT 0
   VAR BottomMargin     INIT 0
   VAR LeftMargin       INIT 0
   VAR RightMargin      INIT 0
   VAR LineHeight       INIT 0
   VAR CharHeight       INIT 0
   VAR CharWidth        INIT 0
   VAR fCharWidth       INIT 0      HIDDEN
   VAR BitmapsOk        INIT .F.
   VAR NumColors        INIT 1
   VAR fDuplexType      INIT 0      HIDDEN            // DMDUP_SIMPLEX, 22/02/2007 change to 0 to use default printer settings
   VAR fPrintQuality    INIT 0      HIDDEN            // DMRES_HIGH, 22/02/2007 change to 0 to use default printer settings
   VAR fNewDuplexType   INIT 0      HIDDEN
   VAR fNewPrintQuality INIT 0      HIDDEN
   VAR fOldLandScape    INIT .F.    HIDDEN
   VAR fOldBinNumber    INIT 0      HIDDEN
   VAR fOldFormType     INIT 0      HIDDEN

   VAR PosX             INIT 0
   VAR PosY             INIT 0

   VAR TextColor
   VAR BkColor
   VAR TextAlign

   VAR BkMode

   VAR hPen             INIT 0
   VAR PenStyle
   VAR PenWidth
   VAR PenColor

ENDCLASS

METHOD New( cPrinter ) CLASS WIN_PRN
   ::PrinterName := iif( Empty( cPrinter ), GetDefaultPrinter(), cPrinter )
   /* Initialized with the current properties of the printer [jarabal] */
   ::GetDocumentProperties()
   RETURN Self

METHOD Create() CLASS WIN_PRN
   LOCAL lResult := .F.
   ::Destroy()                            // Finish current print job if any
   IF ! Empty( ::hPrinterDC := win_CreateDC( ::PrinterName ) )

      // Set Form Type
      // Set Number of Copies
      // Set Orientation
      // Set Duplex mode
      // Set PrintQuality
      win_SetDocumentProperties( ::hPrinterDC, ::PrinterName, ::FormType, ::Landscape, ::Copies, ::BinNumber, ::fDuplexType, ::fPrintQuality )
      // Set mapping mode to pixels, topleft down
      win_SetMapMode( ::hPrinterDC, MM_TEXT )
//    win_SetTextCharacterExtra( ::hPrinterDC, 0 ) // do not add extra char spacing even if bold
      // Get Margins etc... here
      ::PageWidth        := win_GetDeviceCaps( ::hPrinterDC, PHYSICALWIDTH )
      ::PageHeight       := win_GetDeviceCaps( ::hPrinterDC, PHYSICALHEIGHT )
      ::LeftMargin       := win_GetDeviceCaps( ::hPrinterDC, PHYSICALOFFSETX )
      ::RightMargin      := ( ::PageWidth - ::LeftMargin ) + 1
      ::PixelsPerInchY   := win_GetDeviceCaps( ::hPrinterDC, LOGPIXELSY )
      ::PixelsPerInchX   := win_GetDeviceCaps( ::hPrinterDC, LOGPIXELSX )
      ::LineHeight       := Int( ::PixelsPerInchY / 6 )  // Default 6 lines per inch == # of pixels per line
      ::TopMargin        := win_GetDeviceCaps( ::hPrinterDC, PHYSICALOFFSETY )
      ::BottomMargin     := ( ::PageHeight - ::TopMargin ) + 1

      // Set .T. if can print bitmaps
      ::BitMapsOk := win_BitMapsOk( ::hPrinterDC )

      // supports Colour
      ::NumColors := win_GetDeviceCaps( ::hPrinterDC, NUMCOLORS )

      // Set the standard font
      ::SetDefaultFont()
      ::PageNumber := 0
      ::HavePrinted := ::Printing := ::PageInit := .F.
      ::fOldFormType := ::FormType  // Last formtype used
      ::fOldLandScape := ::LandScape
      ::fOldBinNumber := ::BinNumber
      ::fNewDuplexType := ::fDuplexType
      ::fNewPrintQuality := ::fPrintQuality
      lResult := .T.
   ENDIF
   RETURN lResult

METHOD Destroy() CLASS WIN_PRN
   IF ! Empty( ::hPrinterDc )
      IF ::Printing
         ::EndDoc()
      ENDIF
      win_DeleteDC( ::hPrinterDC )
      ::hPrinterDC := NIL
   ENDIF
   RETURN .T.

METHOD PROCEDURE Destruct() CLASS WIN_PRN
   ::Destroy()
   RETURN

METHOD StartDoc( cDocName ) CLASS WIN_PRN
   LOCAL lResult

   DEFAULT cDocName TO win_GetExeFileName() + " [" + DToC( Date() ) + " - " + Time() + "]"

   IF ( lResult := win_StartDoc( ::hPrinterDc, cDocName ) )
      IF !( lResult := ::StartPage( ::hPrinterDc ) )
         ::EndDoc( .T. )
      ELSE
         ::Printing := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD EndDoc( lAbortDoc ) CLASS WIN_PRN
   IF ::HavePrinted
      DEFAULT lAbortDoc TO .F.
   ELSE
      lAbortDoc := .T.
   ENDIF
   IF lAbortDoc
      win_AbortDoc( ::hPrinterDC )
   ELSE
      ::EndPage( .F. )
      win_EndDoc( ::hPrinterDC )
   ENDIF
   ::HavePrinted := ::Printing := ::PageInit := .F.
   ::PageNumber := 0
   RETURN .T.

METHOD StartPage() CLASS WIN_PRN
   LOCAL lLLandScape
   LOCAL nLBinNumber
   LOCAL nLFormType
   LOCAL nLDuplexType
   LOCAL nLPrintQuality
   LOCAL lChangeDP := .F.

   IF ::LandScape != ::fOldLandScape  // Direct-modify property
      lLLandScape := ::fOldLandScape := ::LandScape
      lChangeDP := .T.
   ENDIF
   IF ::BinNumber != ::fOldBinNumber  // Direct-modify property
      nLBinNumber := ::fOldBinNumber := ::BinNumber
      lChangeDP := .T.
   ENDIF
   IF ::FormType != ::fOldFormType  // Direct-modify property
      nLFormType := ::fOldFormType := ::FormType
      lChangeDP := .T.
   ENDIF
   IF ::fDuplexType != ::fNewDuplexType  // Get/Set property
      nLDuplexType := ::fDuplexType := ::fNewDuplexType
      lChangeDP := .T.
   ENDIF
   IF ::fPrintQuality != ::fNewPrintQuality  // Get/Set property
      nLPrintQuality := ::fPrintQuality := ::fNewPrintQuality
      lChangeDP := .T.
   ENDIF
   IF lChangeDP
      win_SetDocumentProperties( ::hPrinterDC, ::PrinterName, nLFormType, lLLandscape, , nLBinNumber, nLDuplexType, nLPrintQuality )
   ENDIF
   win_StartPage( ::hPrinterDC )
   ::PageNumber++
   ::PageInit := .F.
   ::PosX := ::LeftMargin
   ::PosY := ::TopMargin
   RETURN .T.

METHOD CheckPage() CLASS WIN_PRN
   IF ::PageInit
      ::PageInit := .F.
      win_StartPage( ::hPrinterDC )
      ::PageNumber++
      IF win_osIs9X() // Reset font on Win9X
         ::SetFont()
      ENDIF
   ENDIF
   RETURN ::Printing

METHOD EndPage( lStartNewPage ) CLASS WIN_PRN

   DEFAULT lStartNewPage TO .T.

   win_EndPage( ::hPrinterDC )
   IF lStartNewPage
      IF ::PageInit
         ::PosX := ::LeftMargin
         ::PosY := ::TopMargin
      ELSE
         ::StartPage()
         IF win_osIs9X() // Reset font on Win9X
            ::SetFont()
         ENDIF
      ENDIF
   ENDIF

   RETURN .T.

METHOD NewLine() CLASS WIN_PRN
   ::PosX := ::LeftMargin
   ::PosY += ::LineHeight
   RETURN ::PosY

METHOD NewPage( lDelay ) CLASS WIN_PRN

   DEFAULT lDelay TO .F.

   IF ::Printing
      IF lDelay
         ::PageInit := .T.
      ENDIF
      ::EndPage( .T. )
   ENDIF

   RETURN .T.

METHOD GetDocumentProperties() CLASS WIN_PRN
   RETURN win_GetDocumentProperties( ::PrinterName, @::FormType, @::Landscape, @::Copies, @::BinNumber, @::fDuplexType, @::fPrintQuality )

// If font width is specified it is in "characters per inch" to emulate DotMatrix
// An array {nMul,nDiv} is used to get precise size such a the Dot Matric equivalent
// of Compressed print == 16.67 char per inch == { 3,-50 }
// If nDiv is < 0 then Fixed width printing is forced via ExtTextOut()
METHOD SetFont( cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet ) CLASS WIN_PRN
   LOCAL cType
   IF cFontName != NIL
      ::FontName := cFontName
   ENDIF
   IF nPointSize != NIL
      ::FontPointSize := nPointSize
   ENDIF
   IF nWidth != NIL
      cType := ValType( nWidth )
      IF cType == "A"
         ::FontWidth := nWidth
      ELSEIF cType == "N" .AND. ! Empty( nWidth )
         ::FontWidth := { 1, nWidth }
      ELSE
         ::FontWidth := { 0, 0 }
      ENDIF
   ENDIF
   IF nBold != NIL
      ::fBold := nBold
   ENDIF
   IF lUnderLine != NIL
      ::fUnderline:= lUnderLine
   ENDIF
   IF lItalic != NIL
      ::fItalic := lItalic
   ENDIF
   IF nCharSet != NIL
      ::fCharSet := nCharSet
   ENDIF
   IF ( ::SetFontOk := ! Empty( ::hFont := win_CreateFont( ::hPrinterDC, ::FontName, ::FontPointSize, ::FontWidth[ 1 ], ::FontWidth[ 2 ], ::fBold, ::fUnderLine, ::fItalic, ::fCharSet ) ) )
      ::fCharWidth  := ::GetCharWidth()
      ::CharWidth   := Abs( ::fCharWidth )
      ::CharHeight  := ::GetCharHeight()
   ENDIF
   ::FontName := win_GetPrinterFontName( ::hPrinterDC )  // Get the font name that Windows actually used
   RETURN ::SetFontOk

METHOD SetDefaultFont() CLASS WIN_PRN
   RETURN ::SetFont( "Courier New", 12, { 1, 10 }, 0, .F., .F., 0 )

METHOD Bold( nWeight ) CLASS WIN_PRN
   LOCAL nOldValue := ::fBold
   IF nWeight != NIL
      ::fBold := nWeight
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF
   RETURN nOldValue

METHOD Underline( lUnderLine ) CLASS WIN_PRN
   LOCAL lOldValue := ::fUnderline
   IF lUnderLine != NIL
      ::fUnderLine := lUnderLine
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF
   RETURN lOldValue

METHOD Italic( lItalic ) CLASS WIN_PRN
   LOCAL lOldValue := ::fItalic
   IF lItalic != NIL
      ::fItalic := lItalic
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF
   RETURN lOldValue

METHOD CharSet( nCharSet ) CLASS WIN_PRN
   LOCAL nOldValue := ::fCharSet
   IF nCharSet != NIL
      ::fCharSet := nCharSet
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF
   RETURN nOldValue

METHOD SetDuplexType( nDuplexType ) CLASS WIN_PRN
   LOCAL nOldValue := ::fDuplexType
   IF nDuplexType != NIL
      ::fNewDuplexType := nDuplexType
      IF ! ::Printing
         ::fDuplexType := nDuplexType
      ENDIF
   ENDIF
   RETURN nOldValue

METHOD SetPrintQuality( nPrintQuality ) CLASS WIN_PRN
   LOCAL nOldValue := ::fPrintQuality
   IF nPrintQuality != NIL
      ::fNewPrintQuality := nPrintQuality
      IF ! ::Printing
         ::fPrintQuality := nPrintQuality
      ENDIF
   ENDIF
   RETURN nOldValue

METHOD GetFonts() CLASS WIN_PRN
   RETURN win_EnumFonts( ::hPrinterDC )

METHOD SetPos( nPosX, nPosY ) CLASS WIN_PRN
   LOCAL aOldValue := { ::PosX, ::PosY }

   IF nPosX != NIL
      ::PosX := Int( nPosX )
   ENDIF
   IF nPosY != NIL
      ::PosY := Int( nPosY )
   ENDIF

   RETURN aOldValue

METHOD SetColor( nClrText, nClrPane, nAlign ) CLASS WIN_PRN

   IF HB_ISNUMERIC( nClrText )
      ::TextColor := nClrText
   ENDIF
   IF HB_ISNUMERIC( nClrPane )
      ::BkColor := nClrPane
   ENDIF
   IF HB_ISNUMERIC( nAlign )
      ::TextAlign := nAlign
   ENDIF

   RETURN win_SetColor( ::hPrinterDC, nClrText, nClrPane, nAlign )

METHOD SetBkMode( nMode ) CLASS WIN_PRN
   IF HB_ISNUMERIC( nMode )
      ::BkMode := nMode
   ENDIF
   RETURN win_SetBkMode( ::hPrinterDc, nMode )

METHOD TextOut( cString, lNewLine, lUpdatePosX, nAlign ) CLASS WIN_PRN
   LOCAL lResult := .F.
   LOCAL nPosX

   IF cString != NIL .AND. ::CheckPage()

      DEFAULT lNewLine TO .F.
      DEFAULT lUpdatePosX TO .T.
      DEFAULT nAlign TO HB_BITOR( TA_BOTTOM, TA_LEFT )

      nPosX := win_TextOut( ::hPrinterDC, ::PosX, ::PosY, cString, Len( cString ), ::fCharWidth, nAlign )

      ::HavePrinted := lResult := .T.

      IF lUpdatePosX
         ::PosX += nPosX
      ENDIF
      IF lNewLine
         ::NewLine()
      ENDIF

   ENDIF

   RETURN lResult

METHOD TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign ) CLASS WIN_PRN
   ::SetPos( nPosX, nPosY )
   RETURN ::TextOut( cString, lNewLine, lUpdatePosX, nAlign )

METHOD SetPen( nStyle, nWidth, nColor ) CLASS WIN_PRN
   ::PenStyle := nStyle
   ::PenWidth := nWidth
   ::PenColor := nColor
   RETURN ! Empty( ::hPen := win_SetPen( ::hPrinterDC, nStyle, nWidth, nColor ) )

METHOD Line( nX1, nY1, nX2, nY2 ) CLASS WIN_PRN
   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_LineTo( ::hPrinterDC, nX1, nY1, nX2, nY2 )
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD Box( nX1, nY1, nX2, nY2, nWidth, nHeight ) CLASS WIN_PRN
   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_Rectangle( ::hPrinterDC, nX1, nY1, nX2, nY2, nWidth, nHeight )
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD Arc( nX1, nY1, nX2, nY2 ) CLASS WIN_PRN
   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_Arc( ::hPrinterDC, nX1, nY1, nX2, nY2 )
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD Ellipse( nX1, nY1, nX2, nY2 ) CLASS WIN_PRN
   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_Ellipse( ::hPrinterDC, nX1, nY1, nX2, nY2 )
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD FillRect( nX1, nY1, nX2, nY2, nColor ) CLASS WIN_PRN
   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_FillRect( ::hPrinterDC, nX1, nY1, nX2, nY2, nColor )
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD GetCharWidth() CLASS WIN_PRN
   LOCAL nWidth
   IF ::FontWidth[ 2 ] < 0 .AND. ! Empty( ::FontWidth[ 1 ] )
      nWidth := win_MulDiv( ::FontWidth[ 1 ], ::PixelsPerInchX, ::FontWidth[ 2 ] )
   ELSE
      nWidth := win_GetCharSize( ::hPrinterDC )
   ENDIF
   RETURN nWidth

METHOD GetCharHeight() CLASS WIN_PRN
   RETURN win_GetCharSize( ::hPrinterDC, .T. )

METHOD GetTextWidth( cString ) CLASS WIN_PRN
   LOCAL nWidth
   IF ::FontWidth[ 2 ] < 0 .AND. ! Empty( ::FontWidth[ 1 ] )
      nWidth := Len( cString ) * ::CharWidth
   ELSE
      nWidth := win_GetTextSize( ::hPrinterDC, cString, Len( cString ) )  // Return Width in device units
   ENDIF
   RETURN nWidth

METHOD GetTextHeight( cString ) CLASS WIN_PRN
   RETURN win_GetTextSize( ::hPrinterDC, cString, Len( cString ), .F. )  // Return Height in device units

METHOD DrawBitMap( oBmp ) CLASS WIN_PRN
   LOCAL lResult := .F.
   IF ::BitMapsOk .AND. ::CheckPage() .AND. ! Empty( oBmp:BitMap )
      IF ( lResult := win_DrawBitMap( ::hPrinterDc, oBmp:BitMap, oBmp:Rect[ 1 ], oBmp:Rect[ 2 ], oBmp:rect[ 3 ], oBmp:Rect[ 4 ] ) )
         ::HavePrinted := .T.
      ENDIF
   ENDIF
   RETURN lResult

METHOD SetPrc( nRow, nCol ) CLASS WIN_PRN
   ::SetPos( ( nCol * ::CharWidth ) + ::LeftMArgin, ( nRow * ::LineHeight ) + ::TopMargin )
   RETURN NIL

METHOD PRow() CLASS WIN_PRN
   RETURN Int( ( ::PosY- ::TopMargin ) / ::LineHeight )   // No test for Div by ZERO

METHOD PCol() CLASS WIN_PRN
   RETURN Int( ( ::PosX - ::LeftMargin ) / ::CharWidth )   // Uses width of current character

METHOD MaxRow() CLASS WIN_PRN
   RETURN Int( ( ( ::BottomMargin - ::TopMargin ) + 1 ) / ::LineHeight ) - 1

METHOD MaxCol() CLASS WIN_PRN
   RETURN Int( ( ( ::RightMargin - ::LeftMargin ) + 1 ) / ::CharWidth ) - 1

METHOD MM_To_PosX( nMm ) CLASS WIN_PRN
   RETURN Int( ( ( nMM * ::PixelsPerInchX ) / MM_TO_INCH ) - ::LeftMargin )

METHOD MM_To_PosY( nMm ) CLASS WIN_PRN
   RETURN Int( ( ( nMM * ::PixelsPerInchY ) / MM_TO_INCH ) - ::TopMargin )

METHOD Inch_To_PosX( nInch ) CLASS WIN_PRN
   RETURN Int( ( nInch * ::PixelsPerInchX  ) - ::LeftMargin )

METHOD Inch_To_PosY( nInch ) CLASS WIN_PRN
   RETURN Int( ( nInch * ::PixelsPerInchY ) - ::TopMargin )

METHOD TextAtFont( nPosX, nPosY, cString, cFont, nPointSize, nWidth, nBold, lUnderLine, lItalic, nCharSet, lNewLine, lUpdatePosX, nColor, nAlign ) CLASS WIN_PRN
   LOCAL lResult
   LOCAL nDiv := 0
   LOCAL cType
   LOCAL hFont

   IF ::CheckPage()

      DEFAULT nPointSize TO ::FontPointSize

      IF cFont != NIL
         cType := ValType( nWidth )
         IF cType == "A"
            nDiv   := nWidth[ 1 ]
            nWidth := nWidth[ 2 ]
         ELSEIF cType == "N" .AND. ! Empty( nWidth )
            nDiv := 1
         ENDIF
         hFont := ::hFont
         ::hFont := win_CreateFont( ::hPrinterDC, cFont, nPointSize, nDiv, nWidth, nBold, lUnderLine, lItalic, nCharSet )
      ENDIF
      IF nColor != NIL
         nColor := win_SetColor( ::hPrinterDC, nColor )
      ENDIF

      lResult := ::TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign )

      IF cFont != NIL
         ::hFont := hFont     // Reset Font
      ENDIF
      IF nColor != NIL
         win_SetColor( ::hPrinterDC, nColor )  // Reset Color
      ENDIF

   ENDIF
   RETURN lResult

METHOD GetDeviceCaps( nCaps ) CLASS WIN_PRN
   RETURN win_GetDeviceCaps( ::hPrinterDC, nCaps )

// Bitmap class

CREATE CLASS WIN_BMP

   EXPORTED:

   METHOD New()
   METHOD LoadFile( cFileName )
   METHOD Create()
   METHOD Destroy()
   METHOD Draw( oPrn, arectangle )

   VAR Rect     INIT { 0, 0, 0, 0 }     // Coordinates to print BitMap
                                        //   XDest,                    // x-coord of destination upper-left corner
                                        //   YDest,                    // y-coord of destination upper-left corner
                                        //   nDestWidth,               // width of destination rectangle
                                        //   nDestHeight,              // height of destination rectangle
                                        // See WinApi StretchDIBits()
   VAR BitMap   INIT ""
   VAR FileName INIT ""
ENDCLASS

METHOD New() CLASS WIN_BMP
   RETURN Self

METHOD LoadFile( cFileName ) CLASS WIN_BMP
   ::FileName := cFileName
   ::Bitmap := win_LoadBitMapFile( ::FileName )
   RETURN ! Empty( ::Bitmap )

METHOD Create() CLASS WIN_BMP  // Compatibility function for Alaska Xbase++
   RETURN Self

METHOD Destroy() CLASS WIN_BMP  // Compatibility function for Alaska Xbase++
   RETURN NIL

METHOD Draw( oPrn, aRectangle ) CLASS WIN_BMP // Pass a WIN_PRN object reference & Rectangle array
   ::Rect := aRectangle
   RETURN oPrn:DrawBitMap( Self )

#ifdef HB_COMPAT_XPP

/* Compatibility Class for Alaska Xbase++ */

CREATE CLASS XBPBITMAP FROM WIN_BMP
ENDCLASS

#endif
