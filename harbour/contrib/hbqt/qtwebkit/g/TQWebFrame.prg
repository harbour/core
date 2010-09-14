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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QWebFrame( ... )
   RETURN HB_QWebFrame():new( ... )


CREATE CLASS QWebFrame INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QWebFrame

   METHOD  new( ... )

   METHOD  addToJavaScriptWindowObject( cName, pObject )
   METHOD  addToJavaScriptWindowObject_1( cName, pObject, nOwn )
   METHOD  childFrames()
   METHOD  contentsSize()
   METHOD  frameName()
   METHOD  geometry()
   METHOD  hitTestContent( pPos )
   METHOD  icon()
   METHOD  load( pUrl )
   METHOD  page()
   METHOD  parentFrame()
   METHOD  pos()
   METHOD  render( pPainter, pClip )
   METHOD  render_1( pPainter )
   METHOD  renderTreeDump()
   METHOD  scroll( nDx, nDy )
   METHOD  scrollBarMaximum( nOrientation )
   METHOD  scrollBarMinimum( nOrientation )
   METHOD  scrollBarPolicy( nOrientation )
   METHOD  scrollBarValue( nOrientation )
   METHOD  scrollPosition()
   METHOD  securityOrigin()
   METHOD  setContent( pData, cMimeType, pBaseUrl )
   METHOD  setHtml( cHtml, pBaseUrl )
   METHOD  setScrollBarPolicy( nOrientation, nPolicy )
   METHOD  setScrollBarValue( nOrientation, nValue )
   METHOD  setScrollPosition( pPos )
   METHOD  setTextSizeMultiplier( nFactor )
   METHOD  setUrl( pUrl )
   METHOD  setZoomFactor( nFactor )
   METHOD  textSizeMultiplier()
   METHOD  title()
   METHOD  toHtml()
   METHOD  toPlainText()
   METHOD  url()
   METHOD  zoomFactor()
   METHOD  evaluateJavaScript( cScriptSource )
   METHOD  print( pPrinter )

   ENDCLASS


METHOD QWebFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebFrame( ... )
   RETURN Self


METHOD QWebFrame:addToJavaScriptWindowObject( cName, pObject )
   RETURN Qt_QWebFrame_addToJavaScriptWindowObject( ::pPtr, cName, hbqt_ptr( pObject ) )


METHOD QWebFrame:addToJavaScriptWindowObject_1( cName, pObject, nOwn )
   RETURN Qt_QWebFrame_addToJavaScriptWindowObject_1( ::pPtr, cName, hbqt_ptr( pObject ), nOwn )


METHOD QWebFrame:childFrames()
   RETURN Qt_QWebFrame_childFrames( ::pPtr )


METHOD QWebFrame:contentsSize()
   RETURN Qt_QWebFrame_contentsSize( ::pPtr )


METHOD QWebFrame:frameName()
   RETURN Qt_QWebFrame_frameName( ::pPtr )


METHOD QWebFrame:geometry()
   RETURN Qt_QWebFrame_geometry( ::pPtr )


METHOD QWebFrame:hitTestContent( pPos )
   RETURN Qt_QWebFrame_hitTestContent( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWebFrame:icon()
   RETURN Qt_QWebFrame_icon( ::pPtr )


METHOD QWebFrame:load( pUrl )
   RETURN Qt_QWebFrame_load( ::pPtr, hbqt_ptr( pUrl ) )


METHOD QWebFrame:page()
   RETURN Qt_QWebFrame_page( ::pPtr )


METHOD QWebFrame:parentFrame()
   RETURN Qt_QWebFrame_parentFrame( ::pPtr )


METHOD QWebFrame:pos()
   RETURN Qt_QWebFrame_pos( ::pPtr )


METHOD QWebFrame:render( pPainter, pClip )
   RETURN Qt_QWebFrame_render( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pClip ) )


METHOD QWebFrame:render_1( pPainter )
   RETURN Qt_QWebFrame_render_1( ::pPtr, hbqt_ptr( pPainter ) )


METHOD QWebFrame:renderTreeDump()
   RETURN Qt_QWebFrame_renderTreeDump( ::pPtr )


METHOD QWebFrame:scroll( nDx, nDy )
   RETURN Qt_QWebFrame_scroll( ::pPtr, nDx, nDy )


METHOD QWebFrame:scrollBarMaximum( nOrientation )
   RETURN Qt_QWebFrame_scrollBarMaximum( ::pPtr, nOrientation )


METHOD QWebFrame:scrollBarMinimum( nOrientation )
   RETURN Qt_QWebFrame_scrollBarMinimum( ::pPtr, nOrientation )


METHOD QWebFrame:scrollBarPolicy( nOrientation )
   RETURN Qt_QWebFrame_scrollBarPolicy( ::pPtr, nOrientation )


METHOD QWebFrame:scrollBarValue( nOrientation )
   RETURN Qt_QWebFrame_scrollBarValue( ::pPtr, nOrientation )


METHOD QWebFrame:scrollPosition()
   RETURN Qt_QWebFrame_scrollPosition( ::pPtr )


METHOD QWebFrame:securityOrigin()
   RETURN Qt_QWebFrame_securityOrigin( ::pPtr )


METHOD QWebFrame:setContent( pData, cMimeType, pBaseUrl )
   RETURN Qt_QWebFrame_setContent( ::pPtr, hbqt_ptr( pData ), cMimeType, hbqt_ptr( pBaseUrl ) )


METHOD QWebFrame:setHtml( cHtml, pBaseUrl )
   RETURN Qt_QWebFrame_setHtml( ::pPtr, cHtml, hbqt_ptr( pBaseUrl ) )


METHOD QWebFrame:setScrollBarPolicy( nOrientation, nPolicy )
   RETURN Qt_QWebFrame_setScrollBarPolicy( ::pPtr, nOrientation, nPolicy )


METHOD QWebFrame:setScrollBarValue( nOrientation, nValue )
   RETURN Qt_QWebFrame_setScrollBarValue( ::pPtr, nOrientation, nValue )


METHOD QWebFrame:setScrollPosition( pPos )
   RETURN Qt_QWebFrame_setScrollPosition( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWebFrame:setTextSizeMultiplier( nFactor )
   RETURN Qt_QWebFrame_setTextSizeMultiplier( ::pPtr, nFactor )


METHOD QWebFrame:setUrl( pUrl )
   RETURN Qt_QWebFrame_setUrl( ::pPtr, hbqt_ptr( pUrl ) )


METHOD QWebFrame:setZoomFactor( nFactor )
   RETURN Qt_QWebFrame_setZoomFactor( ::pPtr, nFactor )


METHOD QWebFrame:textSizeMultiplier()
   RETURN Qt_QWebFrame_textSizeMultiplier( ::pPtr )


METHOD QWebFrame:title()
   RETURN Qt_QWebFrame_title( ::pPtr )


METHOD QWebFrame:toHtml()
   RETURN Qt_QWebFrame_toHtml( ::pPtr )


METHOD QWebFrame:toPlainText()
   RETURN Qt_QWebFrame_toPlainText( ::pPtr )


METHOD QWebFrame:url()
   RETURN Qt_QWebFrame_url( ::pPtr )


METHOD QWebFrame:zoomFactor()
   RETURN Qt_QWebFrame_zoomFactor( ::pPtr )


METHOD QWebFrame:evaluateJavaScript( cScriptSource )
   RETURN Qt_QWebFrame_evaluateJavaScript( ::pPtr, cScriptSource )


METHOD QWebFrame:print( pPrinter )
   RETURN Qt_QWebFrame_print( ::pPtr, hbqt_ptr( pPrinter ) )

