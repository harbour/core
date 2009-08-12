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


CREATE CLASS QWebView INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QWebView_destroy( ::pPtr )

   METHOD  findText( cSubString, nOptions )    INLINE  Qt_QWebView_findText( ::pPtr, cSubString, nOptions )
   METHOD  history()                           INLINE  Qt_QWebView_history( ::pPtr )
   METHOD  icon()                              INLINE  Qt_QWebView_icon( ::pPtr )
   METHOD  isModified()                        INLINE  Qt_QWebView_isModified( ::pPtr )
   METHOD  load( pUrl )                        INLINE  Qt_QWebView_load( ::pPtr, pUrl )
   METHOD  load_1( pRequest, nOperation, pBody )  INLINE  Qt_QWebView_load_1( ::pPtr, pRequest, nOperation, pBody )
   METHOD  page()                              INLINE  Qt_QWebView_page( ::pPtr )
   METHOD  pageAction( nAction )               INLINE  Qt_QWebView_pageAction( ::pPtr, nAction )
   METHOD  selectedText()                      INLINE  Qt_QWebView_selectedText( ::pPtr )
   METHOD  setContent( pData, cMimeType, pBaseUrl )  INLINE  Qt_QWebView_setContent( ::pPtr, pData, cMimeType, pBaseUrl )
   METHOD  setHtml( cHtml, pBaseUrl )          INLINE  Qt_QWebView_setHtml( ::pPtr, cHtml, pBaseUrl )
   METHOD  setPage( pPage )                    INLINE  Qt_QWebView_setPage( ::pPtr, pPage )
   METHOD  setTextSizeMultiplier( nFactor )    INLINE  Qt_QWebView_setTextSizeMultiplier( ::pPtr, nFactor )
   METHOD  setUrl( pUrl )                      INLINE  Qt_QWebView_setUrl( ::pPtr, pUrl )
   METHOD  setZoomFactor( nFactor )            INLINE  Qt_QWebView_setZoomFactor( ::pPtr, nFactor )
   METHOD  settings()                          INLINE  Qt_QWebView_settings( ::pPtr )
   METHOD  textSizeMultiplier()                INLINE  Qt_QWebView_textSizeMultiplier( ::pPtr )
   METHOD  title()                             INLINE  Qt_QWebView_title( ::pPtr )
   METHOD  triggerPageAction( nAction, lChecked )  INLINE  Qt_QWebView_triggerPageAction( ::pPtr, nAction, lChecked )
   METHOD  url()                               INLINE  Qt_QWebView_url( ::pPtr )
   METHOD  zoomFactor()                        INLINE  Qt_QWebView_zoomFactor( ::pPtr )
   METHOD  back()                              INLINE  Qt_QWebView_back( ::pPtr )
   METHOD  forward()                           INLINE  Qt_QWebView_forward( ::pPtr )
   METHOD  print( pPrinter )                   INLINE  Qt_QWebView_print( ::pPtr, pPrinter )
   METHOD  reload()                            INLINE  Qt_QWebView_reload( ::pPtr )
   METHOD  stop()                              INLINE  Qt_QWebView_stop( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QWebView

   ::pParent := pParent

   ::pPtr := Qt_QWebView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QWebView

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
