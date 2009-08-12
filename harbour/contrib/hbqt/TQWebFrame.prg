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


CREATE CLASS QWebFrame INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QWebFrame_destroy( ::pPtr )

   METHOD  addToJavaScriptWindowObject( cName, pObject )  INLINE  Qt_QWebFrame_addToJavaScriptWindowObject( ::pPtr, cName, pObject )
   METHOD  addToJavaScriptWindowObject_1( cName, pObject, nOwn )  INLINE  Qt_QWebFrame_addToJavaScriptWindowObject_1( ::pPtr, cName, pObject, nOwn )
   METHOD  contentsSize()                      INLINE  Qt_QWebFrame_contentsSize( ::pPtr )
   METHOD  frameName()                         INLINE  Qt_QWebFrame_frameName( ::pPtr )
   METHOD  geometry()                          INLINE  Qt_QWebFrame_geometry( ::pPtr )
   METHOD  hitTestContent( pPos )              INLINE  Qt_QWebFrame_hitTestContent( ::pPtr, pPos )
   METHOD  icon()                              INLINE  Qt_QWebFrame_icon( ::pPtr )
   METHOD  load( pUrl )                        INLINE  Qt_QWebFrame_load( ::pPtr, pUrl )
   METHOD  page()                              INLINE  Qt_QWebFrame_page( ::pPtr )
   METHOD  parentFrame()                       INLINE  Qt_QWebFrame_parentFrame( ::pPtr )
   METHOD  pos()                               INLINE  Qt_QWebFrame_pos( ::pPtr )
   METHOD  render( pPainter, pClip )           INLINE  Qt_QWebFrame_render( ::pPtr, pPainter, pClip )
   METHOD  render_1( pPainter )                INLINE  Qt_QWebFrame_render_1( ::pPtr, pPainter )
   METHOD  renderTreeDump()                    INLINE  Qt_QWebFrame_renderTreeDump( ::pPtr )
   METHOD  scroll( nDx, nDy )                  INLINE  Qt_QWebFrame_scroll( ::pPtr, nDx, nDy )
   METHOD  scrollBarMaximum( nOrientation )    INLINE  Qt_QWebFrame_scrollBarMaximum( ::pPtr, nOrientation )
   METHOD  scrollBarMinimum( nOrientation )    INLINE  Qt_QWebFrame_scrollBarMinimum( ::pPtr, nOrientation )
   METHOD  scrollBarPolicy( nOrientation )     INLINE  Qt_QWebFrame_scrollBarPolicy( ::pPtr, nOrientation )
   METHOD  scrollBarValue( nOrientation )      INLINE  Qt_QWebFrame_scrollBarValue( ::pPtr, nOrientation )
   METHOD  scrollPosition()                    INLINE  Qt_QWebFrame_scrollPosition( ::pPtr )
   METHOD  securityOrigin()                    INLINE  Qt_QWebFrame_securityOrigin( ::pPtr )
   METHOD  setContent( pData, cMimeType, pBaseUrl )  INLINE  Qt_QWebFrame_setContent( ::pPtr, pData, cMimeType, pBaseUrl )
   METHOD  setHtml( cHtml, pBaseUrl )          INLINE  Qt_QWebFrame_setHtml( ::pPtr, cHtml, pBaseUrl )
   METHOD  setScrollBarPolicy( nOrientation, nPolicy )  INLINE  Qt_QWebFrame_setScrollBarPolicy( ::pPtr, nOrientation, nPolicy )
   METHOD  setScrollBarValue( nOrientation, nValue )  INLINE  Qt_QWebFrame_setScrollBarValue( ::pPtr, nOrientation, nValue )
   METHOD  setScrollPosition( pPos )           INLINE  Qt_QWebFrame_setScrollPosition( ::pPtr, pPos )
   METHOD  setTextSizeMultiplier( nFactor )    INLINE  Qt_QWebFrame_setTextSizeMultiplier( ::pPtr, nFactor )
   METHOD  setUrl( pUrl )                      INLINE  Qt_QWebFrame_setUrl( ::pPtr, pUrl )
   METHOD  setZoomFactor( nFactor )            INLINE  Qt_QWebFrame_setZoomFactor( ::pPtr, nFactor )
   METHOD  textSizeMultiplier()                INLINE  Qt_QWebFrame_textSizeMultiplier( ::pPtr )
   METHOD  title()                             INLINE  Qt_QWebFrame_title( ::pPtr )
   METHOD  toHtml()                            INLINE  Qt_QWebFrame_toHtml( ::pPtr )
   METHOD  toPlainText()                       INLINE  Qt_QWebFrame_toPlainText( ::pPtr )
   METHOD  url()                               INLINE  Qt_QWebFrame_url( ::pPtr )
   METHOD  zoomFactor()                        INLINE  Qt_QWebFrame_zoomFactor( ::pPtr )
   METHOD  evaluateJavaScript( cScriptSource )  INLINE  Qt_QWebFrame_evaluateJavaScript( ::pPtr, cScriptSource )
   METHOD  print( pPrinter )                   INLINE  Qt_QWebFrame_print( ::pPtr, pPrinter )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QWebFrame

   ::pParent := pParent

   ::pPtr := Qt_QWebFrame( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QWebFrame

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
