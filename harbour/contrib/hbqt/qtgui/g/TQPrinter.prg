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


FUNCTION QPrinter( ... )
   RETURN HB_QPrinter():new( ... )


CREATE CLASS QPrinter INHERIT HbQtObjectHandler, HB_QPaintDevice FUNCTION HB_QPrinter

   METHOD  new( ... )

   METHOD  abort()
   METHOD  collateCopies()
   METHOD  colorMode()
   METHOD  creator()
   METHOD  docName()
   METHOD  doubleSidedPrinting()
   METHOD  duplex()
   METHOD  fontEmbeddingEnabled()
   METHOD  fromPage()
   METHOD  fullPage()
   METHOD  getPageMargins( nLeft, nTop, nRight, nBottom, nUnit )
   METHOD  isValid()
   METHOD  newPage()
   METHOD  numCopies()
   METHOD  orientation()
   METHOD  outputFileName()
   METHOD  outputFormat()
   METHOD  pageOrder()
   METHOD  pageRect( ... )
   METHOD  paintEngine()
   METHOD  paperRect( ... )
   METHOD  paperSize( ... )
   METHOD  paperSource()
   METHOD  printEngine()
   METHOD  printProgram()
   METHOD  printRange()
   METHOD  printerName()
   METHOD  printerState()
   METHOD  resolution()
   METHOD  setCollateCopies( lCollate )
   METHOD  setColorMode( nNewColorMode )
   METHOD  setCreator( cCreator )
   METHOD  setDocName( cName )
   METHOD  setDoubleSidedPrinting( lDoubleSided )
   METHOD  setDuplex( nDuplex )
   METHOD  setFontEmbeddingEnabled( lEnable )
   METHOD  setFromTo( nFrom, nTo )
   METHOD  setFullPage( lFp )
   METHOD  setNumCopies( nNumCopies )
   METHOD  setOrientation( nOrientation )
   METHOD  setOutputFileName( cFileName )
   METHOD  setOutputFormat( nFormat )
   METHOD  setPageMargins( nLeft, nTop, nRight, nBottom, nUnit )
   METHOD  setPageOrder( nPageOrder )
   METHOD  setPaperSize( ... )
   METHOD  setPaperSource( nSource )
   METHOD  setPrintProgram( cPrintProg )
   METHOD  setPrintRange( nRange )
   METHOD  setPrinterName( cName )
   METHOD  setResolution( nDpi )
   METHOD  supportedResolutions()
   METHOD  toPage()

   ENDCLASS


METHOD QPrinter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPrinter( ... )
   RETURN Self


METHOD QPrinter:abort()
   RETURN Qt_QPrinter_abort( ::pPtr )


METHOD QPrinter:collateCopies()
   RETURN Qt_QPrinter_collateCopies( ::pPtr )


METHOD QPrinter:colorMode()
   RETURN Qt_QPrinter_colorMode( ::pPtr )


METHOD QPrinter:creator()
   RETURN Qt_QPrinter_creator( ::pPtr )


METHOD QPrinter:docName()
   RETURN Qt_QPrinter_docName( ::pPtr )


METHOD QPrinter:doubleSidedPrinting()
   RETURN Qt_QPrinter_doubleSidedPrinting( ::pPtr )


METHOD QPrinter:duplex()
   RETURN Qt_QPrinter_duplex( ::pPtr )


METHOD QPrinter:fontEmbeddingEnabled()
   RETURN Qt_QPrinter_fontEmbeddingEnabled( ::pPtr )


METHOD QPrinter:fromPage()
   RETURN Qt_QPrinter_fromPage( ::pPtr )


METHOD QPrinter:fullPage()
   RETURN Qt_QPrinter_fullPage( ::pPtr )


METHOD QPrinter:getPageMargins( nLeft, nTop, nRight, nBottom, nUnit )
   RETURN Qt_QPrinter_getPageMargins( ::pPtr, nLeft, nTop, nRight, nBottom, nUnit )


METHOD QPrinter:isValid()
   RETURN Qt_QPrinter_isValid( ::pPtr )


METHOD QPrinter:newPage()
   RETURN Qt_QPrinter_newPage( ::pPtr )


METHOD QPrinter:numCopies()
   RETURN Qt_QPrinter_numCopies( ::pPtr )


METHOD QPrinter:orientation()
   RETURN Qt_QPrinter_orientation( ::pPtr )


METHOD QPrinter:outputFileName()
   RETURN Qt_QPrinter_outputFileName( ::pPtr )


METHOD QPrinter:outputFormat()
   RETURN Qt_QPrinter_outputFormat( ::pPtr )


METHOD QPrinter:pageOrder()
   RETURN Qt_QPrinter_pageOrder( ::pPtr )


METHOD QPrinter:pageRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QPrinter_pageRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QRect():from( Qt_QPrinter_pageRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPrinter:paintEngine()
   RETURN HB_QPaintEngine():from( Qt_QPrinter_paintEngine( ::pPtr ) )


METHOD QPrinter:paperRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QPrinter_paperRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QRect():from( Qt_QPrinter_paperRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPrinter:paperSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QSizeF():from( Qt_QPrinter_paperSize_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QPrinter_paperSize( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPrinter:paperSource()
   RETURN Qt_QPrinter_paperSource( ::pPtr )


METHOD QPrinter:printEngine()
   RETURN HB_QPrintEngine():from( Qt_QPrinter_printEngine( ::pPtr ) )


METHOD QPrinter:printProgram()
   RETURN Qt_QPrinter_printProgram( ::pPtr )


METHOD QPrinter:printRange()
   RETURN Qt_QPrinter_printRange( ::pPtr )


METHOD QPrinter:printerName()
   RETURN Qt_QPrinter_printerName( ::pPtr )


METHOD QPrinter:printerState()
   RETURN Qt_QPrinter_printerState( ::pPtr )


METHOD QPrinter:resolution()
   RETURN Qt_QPrinter_resolution( ::pPtr )


METHOD QPrinter:setCollateCopies( lCollate )
   RETURN Qt_QPrinter_setCollateCopies( ::pPtr, lCollate )


METHOD QPrinter:setColorMode( nNewColorMode )
   RETURN Qt_QPrinter_setColorMode( ::pPtr, nNewColorMode )


METHOD QPrinter:setCreator( cCreator )
   RETURN Qt_QPrinter_setCreator( ::pPtr, cCreator )


METHOD QPrinter:setDocName( cName )
   RETURN Qt_QPrinter_setDocName( ::pPtr, cName )


METHOD QPrinter:setDoubleSidedPrinting( lDoubleSided )
   RETURN Qt_QPrinter_setDoubleSidedPrinting( ::pPtr, lDoubleSided )


METHOD QPrinter:setDuplex( nDuplex )
   RETURN Qt_QPrinter_setDuplex( ::pPtr, nDuplex )


METHOD QPrinter:setFontEmbeddingEnabled( lEnable )
   RETURN Qt_QPrinter_setFontEmbeddingEnabled( ::pPtr, lEnable )


METHOD QPrinter:setFromTo( nFrom, nTo )
   RETURN Qt_QPrinter_setFromTo( ::pPtr, nFrom, nTo )


METHOD QPrinter:setFullPage( lFp )
   RETURN Qt_QPrinter_setFullPage( ::pPtr, lFp )


METHOD QPrinter:setNumCopies( nNumCopies )
   RETURN Qt_QPrinter_setNumCopies( ::pPtr, nNumCopies )


METHOD QPrinter:setOrientation( nOrientation )
   RETURN Qt_QPrinter_setOrientation( ::pPtr, nOrientation )


METHOD QPrinter:setOutputFileName( cFileName )
   RETURN Qt_QPrinter_setOutputFileName( ::pPtr, cFileName )


METHOD QPrinter:setOutputFormat( nFormat )
   RETURN Qt_QPrinter_setOutputFormat( ::pPtr, nFormat )


METHOD QPrinter:setPageMargins( nLeft, nTop, nRight, nBottom, nUnit )
   RETURN Qt_QPrinter_setPageMargins( ::pPtr, nLeft, nTop, nRight, nBottom, nUnit )


METHOD QPrinter:setPageOrder( nPageOrder )
   RETURN Qt_QPrinter_setPageOrder( ::pPtr, nPageOrder )


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
   RETURN hbqt_error()


METHOD QPrinter:setPaperSource( nSource )
   RETURN Qt_QPrinter_setPaperSource( ::pPtr, nSource )


METHOD QPrinter:setPrintProgram( cPrintProg )
   RETURN Qt_QPrinter_setPrintProgram( ::pPtr, cPrintProg )


METHOD QPrinter:setPrintRange( nRange )
   RETURN Qt_QPrinter_setPrintRange( ::pPtr, nRange )


METHOD QPrinter:setPrinterName( cName )
   RETURN Qt_QPrinter_setPrinterName( ::pPtr, cName )


METHOD QPrinter:setResolution( nDpi )
   RETURN Qt_QPrinter_setResolution( ::pPtr, nDpi )


METHOD QPrinter:supportedResolutions()
   RETURN HB_QList():from( Qt_QPrinter_supportedResolutions( ::pPtr ) )


METHOD QPrinter:toPage()
   RETURN Qt_QPrinter_toPage( ::pPtr )

