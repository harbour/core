/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible XbpPrinter Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              08Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "xbpdev.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpPrinter

   DATA     oWidget

   DATA     comment                               READONLY
   DATA     devName                               READONLY
   DATA     devParams                             READONLY
   DATA     devPort                               READONLY
   DATA     location                              READONLY
   DATA     spoolFormat                           READONLY

   DATA     collate                               INIT      .F.
   DATA     numCopies                             INIT      1
   DATA     pageRangeSelected                     INIT      {}
   DATA     printRange                            INIT      XBPPDLG_PRINT_ALLPAGES
   DATA     printToFile                           INIT      .F.

   /* Internal */
   DATA     isValid

   METHOD   new()
   METHOD   create( cDeviceName, nSpoolFormat, cDeviceParams )
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()                             VIRTUAL

   METHOD   getHDC()
   METHOD   forms()
   METHOD   list()
   METHOD   paperBins()
   METHOD   paperSize()
   METHOD   printerStatus()
   METHOD   resolution()

   METHOD   setCollationMode( nMode )
   METHOD   setColorMode( nMode )
   METHOD   setDuplexMode( nMode )
   METHOD   setFontMode( nMode )
   METHOD   setFormSize( nFormID )
   METHOD   setNumCopies( nNumCopies )
   METHOD   setOrientation( nOrientation )
   METHOD   setPaperBin( nBin )
   METHOD   setPrintFile( cFileName )
   METHOD   setResolution( anResolution )

   METHOD   setupDialog()

   METHOD   abort()
   METHOD   endDoc()
   METHOD   newPage()
   METHOD   startPage()
   METHOD   endPage()
   METHOD   startDoc( cDocName )

   METHOD   setDevName( cName )                   INLINE ::devName := cName

   DATA     oPrintEngine
   ACCESS   oEngine                               INLINE ::oPrintEngine
   METHOD   getEngineProperty( nProperty )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:new()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:create( cDeviceName, nSpoolFormat, cDeviceParams )

   HB_SYMBOL_UNUSED( nSpoolFormat )
   HB_SYMBOL_UNUSED( cDeviceParams )

   ::oWidget := QPrinter()

   IF hb_isChar( cDeviceName )
      ::oWidget:setPrinterName( cDeviceName )
   ENDIF
   IF !::oWidget:isValid()
      //::oWidget:setPrinterName( const QString & name )
      ::oWidget:setPrinterName( "" )
   ENDIF

   ::devName := ::oWidget:printerName()
   ::oPrintEngine := ::oWidget:printEngine()

   #if 0
   ::oWidget:setCreator( const QString & creator )
   ::oWidget:setDoubleSidedPrinting( bool doubleSided )
   ::oWidget:setFromTo( int from, int to )
   ::oWidget:setFullPage( bool fp )
   ::oWidget:setOutputFormat( OutputFormat format )
   ::oWidget:setPageMargins( qreal left, qreal top, qreal right, qreal bottom, Unit unit )
   ::oWidget:setPageOrder( PageOrder pageOrder )
   ::oWidget:setPrintProgram( const QString & printProg )
   ::oWidget:setPrintRange( PrintRange range )
   ::oWidget:setPrinterSelectionOption( const QString & option )
   ::oWidget:setWinPageSize( int pageSize )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:getHDC()
   LOCAL hDC := NIL
   RETURN hDC

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:forms()
   LOCAL aForms := {}
   RETURN aForms

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:list()
   LOCAL aPrinterNames := {}
   RETURN aPrinterNames

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:paperBins()
   LOCAL aPaperBins := {}
   RETURN aPaperBins

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:paperSize()
   LOCAL aPaperSize := {}
   //::oWidget:setPaperSize( PaperSize newPaperSize )
   //::oWidget:setPaperSize( const QSizeF & paperSize, Unit unit )
   RETURN aPaperSize

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:printerStatus()
   LOCAL nStatus := ::oWidget:printerState()
   RETURN nStatus

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:resolution()
   LOCAL aResolution := {}


   RETURN aResolution

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setCollationMode( nMode )
   LOCAL nModeOld := IF( ::oWidget:collateCopies(), XBPPRN_COLLATIONMODE_ON, XBPPRN_COLLATIONMODE_OFF )

   IF hb_isNumeric( nMode )
      ::oWidget:setCollateCopies( IF( nMode == XBPPRN_COLLATIONMODE_ON, .t., .f. ) )
   ENDIF

   RETURN nModeOld

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setColorMode( nMode )
   LOCAL nModeOld := IF( ::oWidget:colorMode() == QPrinter_Color, XBPPRN_COLORMODE_ON, XBPPRN_COLORMODE_OFF )

   IF hb_isNumeric( nMode )
      ::oWidget:setColorMode( IF( nMode == XBPPRN_COLORMODE_ON, QPrinter_Color, QPrinter_GrayScale ) )
   ENDIF

   RETURN nModeOld

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setDuplexMode( nMode )
   LOCAL nModeOld := ::oWidget:duplex()

   IF nModeOld == QPrinter_DuplexNone .OR. nModeOld == QPrinter_DuplexAuto
      nModeOld := XBPPRN_DUPLEXMODE_OFF
   ELSEIF nModeOld == QPrinter_DuplexLongSide
      nModeOld := XBPPRN_DUPLEXMODE_MEMO
   ELSEIF nModeOld == QPrinter_DuplexShortSide
      nModeOld := XBPPRN_DUPLEXMODE_BOOK
   ENDIF

   IF hb_isNumeric( nMode )
      IF nMode == XBPPRN_DUPLEXMODE_MEMO
         nMode := QPrinter_DuplexLongSide
      ELSEIF nMode == XBPPRN_DUPLEXMODE_BOOK
         nMode := QPrinter_DuplexShortSide
      ELSE
         nMode := QPrinter_DuplexNone
      ENDIF

      ::oWidget:setDuplex( nMode )
   ENDIF

   RETURN nModeOld

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setFontMode( nMode )
   LOCAL nModeOld := nMode

   //::oWidget:setFontEmbeddingEnabled( bool enable )

   RETURN nModeOld

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setFormSize( nFormID )
   LOCAL nFormIDOld := ::oWidget:paperSize()
   #if 0
   LOCAL q_:={}
   LOCAL x_:={}
   LOCAL n

   aadd( q_, QPrinter_A0        )
   aadd( q_, QPrinter_A1        )
   aadd( q_, QPrinter_A2       , XBPPRN_FORM_A2 )
   aadd( q_, QPrinter_A3       , XBPPRN_FORM_A3 )
   aadd( q_, QPrinter_A4       , XBPPRN_FORM_A4 )
   aadd( q_, QPrinter_A5       , XBPPRN_FORM_A5 )
   aadd( q_, QPrinter_A6        )
   aadd( q_, QPrinter_A7        )
   aadd( q_, QPrinter_A8        )
   aadd( q_, QPrinter_A9        )
   aadd( q_, QPrinter_B0        )
   aadd( q_, QPrinter_B1        )
   aadd( q_, QPrinter_B2        )
   aadd( q_, QPrinter_B3        )
   aadd( q_, QPrinter_B4       , XBPPRN_FORM_B4 )
   aadd( q_, QPrinter_B5       , XBPPRN_FORM_B5 )
   aadd( q_, QPrinter_B6        )
   aadd( q_, QPrinter_B7        )
   aadd( q_, QPrinter_B8        )
   aadd( q_, QPrinter_B9        )
   aadd( q_, QPrinter_B10       )
   aadd( q_, QPrinter_Letter   , XBPPRN_FORM_LETTER    )
   aadd( q_, QPrinter_Legal    , XBPPRN_FORM_LEGAL     )
   aadd( q_, QPrinter_Executive, XBPPRN_FORM_EXECUTIVE )
   aadd( q_, QPrinter_C5E       )
   aadd( q_, QPrinter_Comm10E  , XBPPRN_FORM_ENVELOPE_10 )
   aadd( q_, QPrinter_DLE       )
   aadd( q_, QPrinter_Folio    , XBPPRN_FORM_FOLIO     )
   aadd( q_, QPrinter_Ledger   , XBPPRN_FORM_LEDGER    )
   aadd( q_, QPrinter_Tabloid  , XBPPRN_FORM_TABLOID   )
   aadd( q_, QPrinter_Custom    )


   XBPPRN_FORM_LETTERSMALL
   XBPPRN_FORM_STATEMENT
   XBPPRN_FORM_A4SMALL
   XBPPRN_FORM_QUARTO
   XBPPRN_FORM_10X14
   XBPPRN_FORM_11X17
   XBPPRN_FORM_NOTE
   XBPPRN_FORM_ENVELOPE_9
   XBPPRN_FORM_ENVELOPE_11
   XBPPRN_FORM_ENVELOPE_12
   XBPPRN_FORM_ENVELOPE_14
   XBPPRN_FORM_CSHEET
   XBPPRN_FORM_DSHEET
   XBPPRN_FORM_ESHEET
   XBPPRN_FORM_ENVELOPE_DL
   XBPPRN_FORM_ENVELOPE_C5
   XBPPRN_FORM_ENVELOPE_C3
   XBPPRN_FORM_ENVELOPE_C4
   XBPPRN_FORM_ENVELOPE_C6
   XBPPRN_FORM_ENVELOPE_C65
   XBPPRN_FORM_ENVELOPE_B4
   XBPPRN_FORM_ENVELOPE_B5
   XBPPRN_FORM_ENVELOPE_B6
   XBPPRN_FORM_ENVELOPE_ITALY
   XBPPRN_FORM_ENVELOPE_MONARCH
   XBPPRN_FORM_ENVELOPE_PERS
   XBPPRN_FORM_FANFOLD_US
   XBPPRN_FORM_FANFOLD_GER
   XBPPRN_FORM_FANFOLD_LGL_GER
   XBPPRN_FORM_ISO_B4
   XBPPRN_FORM_JAPANESE_POSTCARD
   XBPPRN_FORM_9X11
   XBPPRN_FORM_10X11
   XBPPRN_FORM_15X11
   XBPPRN_FORM_ENVELOPE_INV
   XBPPRN_FORM_LETTER_EXTRA
   XBPPRN_FORM_LEGAL_EXTRA
   XBPPRN_FORM_TABLOID_EXTRA
   XBPPRN_FORM_A4_EXTRA
   XBPPRN_FORM_LETTER_TRANSVERSE
   XBPPRN_FORM_A4_TRANSVERSE
   XBPPRN_FORM_LETTER_XTRA_TRANS
   XBPPRN_FORM_A_PLUS
   XBPPRN_FORM_B_PLUS
   XBPPRN_FORM_LETTER_PLUS
   XBPPRN_FORM_A4_PLUS
   XBPPRN_FORM_A5_TRANSVERSE
   XBPPRN_FORM_B5_TRANSVERSE
   XBPPRN_FORM_A3_EXTRA
   XBPPRN_FORM_A5_EXTRA
   XBPPRN_FORM_B5_EXTRA
   XBPPRN_FORM_A3_TRANSVERSE
   XBPPRN_FORM_A3_EXTRA_TRANS
   #endif

   IF hb_isNumeric( nFormID )
      ::oWidget:setPaperSize( nFormID )
   ENDIF
   //::oWidget:setPaperSize( const QSizeF & paperSize, Unit unit )

   RETURN nFormIDOld

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setNumCopies( nNumCopies )
   //LOCAL nOldNumCopies := ::oWidget:numCopies()
   LOCAL nOldNumCopies := ::getEngineProperty( QPrintEngine_PPK_NumberOfCopies )

   IF hb_isNumeric( nNumCopies )
      ::oWidget:setNumCopies( nNumCopies )
   ENDIF

   RETURN nOldNumCopies

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setOrientation( nOrientation )
   LOCAL nOldOrientation := IF( ::oWidget:orientation() == QPrinter_Landscape, XBPPRN_ORIENT_LANDSCAPE, XBPPRN_ORIENT_PORTRAIT )

   IF hb_isNumeric( nOrientation )
      ::oWidget:setOrientation( if( nOrientation == XBPPRN_ORIENT_LANDSCAPE, QPrinter_Landscape, QPrinter_Portrait ) )
   ENDIF

   RETURN nOldOrientation

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setPaperBin( nBin )
   LOCAL q_:= { }
   LOCAL x_:= {}
   LOCAL nOldBin

   aadd( q_, QPrinter_OnlyOne        )
   aadd( q_, QPrinter_Lower          )
   aadd( q_, QPrinter_Middle         )
   aadd( q_, QPrinter_Manual         )
   aadd( q_, QPrinter_Envelope       )
   aadd( q_, QPrinter_EnvelopeManual )
   aadd( q_, QPrinter_Auto           )
   aadd( q_, QPrinter_Tractor        )
   aadd( q_, QPrinter_SmallFormat    )
   aadd( q_, QPrinter_LargeFormat    )
   aadd( q_, QPrinter_LargeCapacity  )
   aadd( q_, QPrinter_Cassette       )
   aadd( q_, QPrinter_FormSource     )

   //aadd( q_, QPrinter_MaxPageSource  ) // Xbase++ does not define it

   aadd( x_, XBPPRN_PAPERBIN_SINGLE        )
   aadd( x_, XBPPRN_PAPERBIN_LOWER         )
   aadd( x_, XBPPRN_PAPERBIN_MIDDLE        )
   aadd( x_, XBPPRN_PAPERBIN_MANUAL        )
   aadd( x_, XBPPRN_PAPERBIN_ENVELOPE      )
   aadd( x_, XBPPRN_PAPERBIN_ENVMANUAL     )
   aadd( x_, XBPPRN_PAPERBIN_AUTO          )
   aadd( x_, XBPPRN_PAPERBIN_TRACTOR       )
   aadd( x_, XBPPRN_PAPERBIN_SMALLFORMAT   )
   aadd( x_, XBPPRN_PAPERBIN_LARGEFORMAT   )
   aadd( x_, XBPPRN_PAPERBIN_LARGECAPACITY )
   aadd( x_, XBPPRN_PAPERBIN_CASETTE       )
   aadd( x_, XBPPRN_PAPERBIN_FORMSOURCE    )

   nOldBin := ::oWidget:paperSource()

   nOldBin := x_[ ascan( q_, nOldBin ) ]

   IF hb_isNumeric( nBin )
      nBin := q_[ ascan( x_, nBin ) ]
      ::oWidget:setPaperSource( nBin )
   ENDIF

   RETURN nOldBin

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setPrintFile( cFileName )
   LOCAL cOldFileName := ::oWidget:outputFileName()

   IF hb_isChar( cFileName )
      ::oWidget:setOutputFileName( cFileName )
   ENDIF

   RETURN cOldFileName

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setResolution( anResolution )
   LOCAL aOldResolution := { ::getEngineProperty( QPrintEngine_PPK_Resolution ), ::oWidget:resolution() }

   IF hb_isNumeric( anResolution ) .or. hb_isArray( anResolution )
      ::oWidget:setResolution( IF( hb_isNumeric( anResolution ), anResolution, anResolution[ 1 ] ) )
   ENDIF

   RETURN aOldResolution

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:setupDialog()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:abort()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:endDoc()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:newPage()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:startPage()
   LOCAL lSuccess := .T.
   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:endPage()
   LOCAL lSuccess := .T.
   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:startDoc( cDocName )

   IF !empty( cDocName )
   //::oWidget:setDocName( const QString & name )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrinter:getEngineProperty( nProperty )
   LOCAL oVariant := HB_QVariant():from( ::oEngine:property( nProperty ) )

   DO CASE
   CASE nProperty == QPrintEngine_PPK_CollateCopies
      RETURN oVariant:toBool()
   CASE nProperty == QPrintEngine_PPK_ColorMode
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_Creator
   CASE nProperty == QPrintEngine_PPK_Duplex
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_DocumentName
      RETURN oVariant:toString()
   CASE nProperty == QPrintEngine_PPK_FontEmbedding
   CASE nProperty == QPrintEngine_PPK_FullPage
      RETURN oVariant:toBool()
   CASE nProperty == QPrintEngine_PPK_NumberOfCopies
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_Orientation
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_OutputFileName
      RETURN oVariant:toString()
   CASE nProperty == QPrintEngine_PPK_PageOrder
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_PageRect
   CASE nProperty == QPrintEngine_PPK_PageSize
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_PaperRect
   CASE nProperty == QPrintEngine_PPK_PaperSource
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_PaperSources
   CASE nProperty == QPrintEngine_PPK_PaperSize
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_PrinterName
      RETURN oVariant:toString()
   CASE nProperty == QPrintEngine_PPK_PrinterProgram
      RETURN oVariant:toString()
   CASE nProperty == QPrintEngine_PPK_Resolution
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_SelectionOption
      RETURN oVariant:toInt()
   CASE nProperty == QPrintEngine_PPK_SupportedResolutions
   CASE nProperty == QPrintEngine_PPK_SuppressSystemPrintStatus
   CASE nProperty == QPrintEngine_PPK_WindowsPageSize
   CASE nProperty == QPrintEngine_PPK_CustomPaperSize
   CASE nProperty == QPrintEngine_PPK_PageMargins
   ENDCASE

   RETURN 0

/*----------------------------------------------------------------------*/
