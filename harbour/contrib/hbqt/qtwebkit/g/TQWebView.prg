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


FUNCTION QWebView( ... )
   RETURN HB_QWebView():new( ... )

FUNCTION QWebViewFromPointer( ... )
   RETURN HB_QWebView():fromPointer( ... )


CREATE CLASS QWebView INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QWebView

   METHOD  new( ... )

   METHOD  findText                      // ( cSubString, nOptions )                           -> lBool
   METHOD  history                       // (  )                                               -> oQWebHistory
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  isModified                    // (  )                                               -> lBool
   METHOD  load                          // ( oQUrl )                                          -> NIL
                                         // ( oQNetworkRequest, nOperation, oQByteArray )      -> NIL
   METHOD  page                          // (  )                                               -> oQWebPage
   METHOD  pageAction                    // ( nAction )                                        -> oQAction
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  setContent                    // ( oQByteArray, cMimeType, oQUrl )                  -> NIL
   METHOD  setHtml                       // ( cHtml, oQUrl )                                   -> NIL
   METHOD  setPage                       // ( oQWebPage )                                      -> NIL
   METHOD  setTextSizeMultiplier         // ( nFactor )                                        -> NIL
   METHOD  setUrl                        // ( oQUrl )                                          -> NIL
   METHOD  setZoomFactor                 // ( nFactor )                                        -> NIL
   METHOD  settings                      // (  )                                               -> oQWebSettings
   METHOD  textSizeMultiplier            // (  )                                               -> nQreal
   METHOD  title                         // (  )                                               -> cQString
   METHOD  triggerPageAction             // ( nAction, lChecked )                              -> NIL
   METHOD  url                           // (  )                                               -> oQUrl
   METHOD  zoomFactor                    // (  )                                               -> nQreal
   METHOD  back                          // (  )                                               -> NIL
   METHOD  forward                       // (  )                                               -> NIL
   METHOD  print                         // ( oQPrinter )                                      -> NIL
   METHOD  reload                        // (  )                                               -> NIL
   METHOD  stop                          // (  )                                               -> NIL

   ENDCLASS


METHOD QWebView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebView( ... )
   RETURN Self


METHOD QWebView:findText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebView_findText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_findText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:history( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebHistoryFromPointer( Qt_QWebView_history( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QWebView_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:isModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_isModified( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:load( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QWebView_load_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebView_load_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QURL"
            RETURN Qt_QWebView_load( ::pPtr, ... )
         CASE "QNETWORKREQUEST"
            RETURN Qt_QWebView_load_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:page( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebPageFromPointer( Qt_QWebView_page( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:pageAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QWebView_pageAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:setContent( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QWebView_setContent( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWebView_setContent( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_setContent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:setHtml( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWebView_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:setPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_setPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:setTextSizeMultiplier( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_setTextSizeMultiplier( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:setUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_setUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:setZoomFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_setZoomFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:settings( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebSettingsFromPointer( Qt_QWebView_settings( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:textSizeMultiplier( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_textSizeMultiplier( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:triggerPageAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWebView_triggerPageAction( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_triggerPageAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:url( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebView_url( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:zoomFactor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_zoomFactor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:forward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_forward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:print( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebView_print( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:reload( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_reload( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebView:stop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebView_stop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

