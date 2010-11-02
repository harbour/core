/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTWEBKIT


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

