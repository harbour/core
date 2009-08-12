/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


CREATE CLASS QPrinter INHERIT QPaintDevice

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QPrinter_destroy( ::pPtr )

   METHOD  abort()                             INLINE  Qt_QPrinter_abort( ::pPtr )
   METHOD  collateCopies()                     INLINE  Qt_QPrinter_collateCopies( ::pPtr )
   METHOD  colorMode()                         INLINE  Qt_QPrinter_colorMode( ::pPtr )
   METHOD  creator()                           INLINE  Qt_QPrinter_creator( ::pPtr )
   METHOD  docName()                           INLINE  Qt_QPrinter_docName( ::pPtr )
   METHOD  doubleSidedPrinting()               INLINE  Qt_QPrinter_doubleSidedPrinting( ::pPtr )
   METHOD  duplex()                            INLINE  Qt_QPrinter_duplex( ::pPtr )
   METHOD  fontEmbeddingEnabled()              INLINE  Qt_QPrinter_fontEmbeddingEnabled( ::pPtr )
   METHOD  fromPage()                          INLINE  Qt_QPrinter_fromPage( ::pPtr )
   METHOD  fullPage()                          INLINE  Qt_QPrinter_fullPage( ::pPtr )
   METHOD  getPageMargins( nLeft, nTop, nRight, nBottom, nUnit )  INLINE  Qt_QPrinter_getPageMargins( ::pPtr, nLeft, nTop, nRight, nBottom, nUnit )
   METHOD  isValid()                           INLINE  Qt_QPrinter_isValid( ::pPtr )
   METHOD  newPage()                           INLINE  Qt_QPrinter_newPage( ::pPtr )
   METHOD  numCopies()                         INLINE  Qt_QPrinter_numCopies( ::pPtr )
   METHOD  orientation()                       INLINE  Qt_QPrinter_orientation( ::pPtr )
   METHOD  outputFileName()                    INLINE  Qt_QPrinter_outputFileName( ::pPtr )
   METHOD  outputFormat()                      INLINE  Qt_QPrinter_outputFormat( ::pPtr )
   METHOD  pageOrder()                         INLINE  Qt_QPrinter_pageOrder( ::pPtr )
   METHOD  pageRect()                          INLINE  Qt_QPrinter_pageRect( ::pPtr )
   METHOD  pageRect_1( nUnit )                 INLINE  Qt_QPrinter_pageRect_1( ::pPtr, nUnit )
   METHOD  paintEngine()                       INLINE  Qt_QPrinter_paintEngine( ::pPtr )
   METHOD  paperRect()                         INLINE  Qt_QPrinter_paperRect( ::pPtr )
   METHOD  paperRect_1( nUnit )                INLINE  Qt_QPrinter_paperRect_1( ::pPtr, nUnit )
   METHOD  paperSize()                         INLINE  Qt_QPrinter_paperSize( ::pPtr )
   METHOD  paperSize_1( nUnit )                INLINE  Qt_QPrinter_paperSize_1( ::pPtr, nUnit )
   METHOD  paperSource()                       INLINE  Qt_QPrinter_paperSource( ::pPtr )
   METHOD  printEngine()                       INLINE  Qt_QPrinter_printEngine( ::pPtr )
   METHOD  printProgram()                      INLINE  Qt_QPrinter_printProgram( ::pPtr )
   METHOD  printRange()                        INLINE  Qt_QPrinter_printRange( ::pPtr )
   METHOD  printerName()                       INLINE  Qt_QPrinter_printerName( ::pPtr )
   METHOD  printerState()                      INLINE  Qt_QPrinter_printerState( ::pPtr )
   METHOD  resolution()                        INLINE  Qt_QPrinter_resolution( ::pPtr )
   METHOD  setCollateCopies( lCollate )        INLINE  Qt_QPrinter_setCollateCopies( ::pPtr, lCollate )
   METHOD  setColorMode( nNewColorMode )       INLINE  Qt_QPrinter_setColorMode( ::pPtr, nNewColorMode )
   METHOD  setCreator( cCreator )              INLINE  Qt_QPrinter_setCreator( ::pPtr, cCreator )
   METHOD  setDocName( cName )                 INLINE  Qt_QPrinter_setDocName( ::pPtr, cName )
   METHOD  setDoubleSidedPrinting( lDoubleSided )  INLINE  Qt_QPrinter_setDoubleSidedPrinting( ::pPtr, lDoubleSided )
   METHOD  setDuplex( nDuplex )                INLINE  Qt_QPrinter_setDuplex( ::pPtr, nDuplex )
   METHOD  setFontEmbeddingEnabled( lEnable )  INLINE  Qt_QPrinter_setFontEmbeddingEnabled( ::pPtr, lEnable )
   METHOD  setFromTo( nFrom, nTo )             INLINE  Qt_QPrinter_setFromTo( ::pPtr, nFrom, nTo )
   METHOD  setFullPage( lFp )                  INLINE  Qt_QPrinter_setFullPage( ::pPtr, lFp )
   METHOD  setNumCopies( nNumCopies )          INLINE  Qt_QPrinter_setNumCopies( ::pPtr, nNumCopies )
   METHOD  setOrientation( nOrientation )      INLINE  Qt_QPrinter_setOrientation( ::pPtr, nOrientation )
   METHOD  setOutputFileName( cFileName )      INLINE  Qt_QPrinter_setOutputFileName( ::pPtr, cFileName )
   METHOD  setOutputFormat( nFormat )          INLINE  Qt_QPrinter_setOutputFormat( ::pPtr, nFormat )
   METHOD  setPageMargins( nLeft, nTop, nRight, nBottom, nUnit )  INLINE  Qt_QPrinter_setPageMargins( ::pPtr, nLeft, nTop, nRight, nBottom, nUnit )
   METHOD  setPageOrder( nPageOrder )          INLINE  Qt_QPrinter_setPageOrder( ::pPtr, nPageOrder )
   METHOD  setPaperSize( nNewPaperSize )       INLINE  Qt_QPrinter_setPaperSize( ::pPtr, nNewPaperSize )
   METHOD  setPaperSize_1( pPaperSize, nUnit )  INLINE  Qt_QPrinter_setPaperSize_1( ::pPtr, pPaperSize, nUnit )
   METHOD  setPaperSource( nSource )           INLINE  Qt_QPrinter_setPaperSource( ::pPtr, nSource )
   METHOD  setPrintProgram( cPrintProg )       INLINE  Qt_QPrinter_setPrintProgram( ::pPtr, cPrintProg )
   METHOD  setPrintRange( nRange )             INLINE  Qt_QPrinter_setPrintRange( ::pPtr, nRange )
   METHOD  setPrinterName( cName )             INLINE  Qt_QPrinter_setPrinterName( ::pPtr, cName )
   METHOD  setResolution( nDpi )               INLINE  Qt_QPrinter_setResolution( ::pPtr, nDpi )
   METHOD  toPage()                            INLINE  Qt_QPrinter_toPage( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QPrinter

   ::pParent := pParent

   ::pPtr := Qt_QPrinter( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QPrinter

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
