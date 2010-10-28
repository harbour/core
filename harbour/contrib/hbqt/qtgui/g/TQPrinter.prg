/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QPrinter( ... )
   RETURN HB_QPrinter():new( ... )

FUNCTION QPrinterFromPointer( ... )
   RETURN HB_QPrinter():fromPointer( ... )


CREATE CLASS QPrinter INHERIT HbQtObjectHandler, HB_QPaintDevice FUNCTION HB_QPrinter

   METHOD  new( ... )

   METHOD  abort                         // (  )                                               -> lBool
   METHOD  collateCopies                 // (  )                                               -> lBool
   METHOD  colorMode                     // (  )                                               -> nColorMode
   METHOD  creator                       // (  )                                               -> cQString
   METHOD  docName                       // (  )                                               -> cQString
   METHOD  doubleSidedPrinting           // (  )                                               -> lBool
   METHOD  duplex                        // (  )                                               -> nDuplexMode
   METHOD  fontEmbeddingEnabled          // (  )                                               -> lBool
   METHOD  fromPage                      // (  )                                               -> nInt
   METHOD  fullPage                      // (  )                                               -> lBool
   METHOD  getPageMargins                // ( @nLeft, @nTop, @nRight, @nBottom, nUnit )        -> NIL
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  newPage                       // (  )                                               -> lBool
   METHOD  numCopies                     // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nOrientation
   METHOD  outputFileName                // (  )                                               -> cQString
   METHOD  outputFormat                  // (  )                                               -> nOutputFormat
   METHOD  pageOrder                     // (  )                                               -> nPageOrder
   METHOD  pageRect                      // (  )                                               -> oQRect
                                         // ( nUnit )                                          -> oQRectF
   METHOD  paintEngine                   // (  )                                               -> oQPaintEngine
   METHOD  paperRect                     // (  )                                               -> oQRect
                                         // ( nUnit )                                          -> oQRectF
   METHOD  paperSize                     // (  )                                               -> nPaperSize
                                         // ( nUnit )                                          -> oQSizeF
   METHOD  paperSource                   // (  )                                               -> nPaperSource
   METHOD  printEngine                   // (  )                                               -> oQPrintEngine
   METHOD  printProgram                  // (  )                                               -> cQString
   METHOD  printRange                    // (  )                                               -> nPrintRange
   METHOD  printerName                   // (  )                                               -> cQString
   METHOD  printerState                  // (  )                                               -> nPrinterState
   METHOD  resolution                    // (  )                                               -> nInt
   METHOD  setCollateCopies              // ( lCollate )                                       -> NIL
   METHOD  setColorMode                  // ( nNewColorMode )                                  -> NIL
   METHOD  setCreator                    // ( cCreator )                                       -> NIL
   METHOD  setDocName                    // ( cName )                                          -> NIL
   METHOD  setDoubleSidedPrinting        // ( lDoubleSided )                                   -> NIL
   METHOD  setDuplex                     // ( nDuplex )                                        -> NIL
   METHOD  setFontEmbeddingEnabled       // ( lEnable )                                        -> NIL
   METHOD  setFromTo                     // ( nFrom, nTo )                                     -> NIL
   METHOD  setFullPage                   // ( lFp )                                            -> NIL
   METHOD  setNumCopies                  // ( nNumCopies )                                     -> NIL
   METHOD  setOrientation                // ( nOrientation )                                   -> NIL
   METHOD  setOutputFileName             // ( cFileName )                                      -> NIL
   METHOD  setOutputFormat               // ( nFormat )                                        -> NIL
   METHOD  setPageMargins                // ( nLeft, nTop, nRight, nBottom, nUnit )            -> NIL
   METHOD  setPageOrder                  // ( nPageOrder )                                     -> NIL
   METHOD  setPaperSize                  // ( nNewPaperSize )                                  -> NIL
                                         // ( oQSizeF, nUnit )                                 -> NIL
   METHOD  setPaperSource                // ( nSource )                                        -> NIL
   METHOD  setPrintProgram               // ( cPrintProg )                                     -> NIL
   METHOD  setPrintRange                 // ( nRange )                                         -> NIL
   METHOD  setPrinterName                // ( cName )                                          -> NIL
   METHOD  setResolution                 // ( nDpi )                                           -> NIL
   METHOD  supportedResolutions          // (  )                                               -> oQList_int>
   METHOD  toPage                        // (  )                                               -> nInt

   ENDCLASS


METHOD QPrinter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPrinter( ... )
   RETURN Self


METHOD QPrinter:abort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_abort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:collateCopies( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_collateCopies( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:colorMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_colorMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:creator( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_creator( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:docName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_docName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:doubleSidedPrinting( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_doubleSidedPrinting( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:duplex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_duplex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:fontEmbeddingEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_fontEmbeddingEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:fromPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_fromPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:fullPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_fullPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:getPageMargins( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPrinter_getPageMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:newPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_newPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:numCopies( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_numCopies( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:outputFileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_outputFileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:outputFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_outputFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:pageOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_pageOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:pageRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QPrinter_pageRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QRectFromPointer( Qt_QPrinter_pageRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:paintEngine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintEngineFromPointer( Qt_QPrinter_paintEngine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:paperRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QPrinter_paperRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QRectFromPointer( Qt_QPrinter_paperRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:paperSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QSizeFFromPointer( Qt_QPrinter_paperSize_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QPrinter_paperSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:paperSource( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_paperSource( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:printEngine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPrintEngineFromPointer( Qt_QPrinter_printEngine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:printProgram( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_printProgram( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:printRange( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_printRange( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:printerName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_printerName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:printerState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_printerState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:resolution( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_resolution( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setCollateCopies( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setCollateCopies( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setColorMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setColorMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setCreator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setCreator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setDocName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setDocName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setDoubleSidedPrinting( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setDoubleSidedPrinting( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setDuplex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setDuplex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setFontEmbeddingEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setFontEmbeddingEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setFromTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPrinter_setFromTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setFullPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setFullPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setNumCopies( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setNumCopies( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setOutputFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setOutputFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setOutputFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setOutputFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPageMargins( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPrinter_setPageMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPageOrder( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setPageOrder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPaperSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPrinter_setPaperSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setPaperSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPaperSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setPaperSource( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPrintProgram( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setPrintProgram( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPrintRange( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setPrintRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setPrinterName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setPrinterName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:setResolution( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrinter_setResolution( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:supportedResolutions( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QPrinter_supportedResolutions( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinter:toPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinter_toPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

