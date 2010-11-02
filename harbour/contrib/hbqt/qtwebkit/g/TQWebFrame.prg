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


FUNCTION QWebFrame( ... )
   RETURN HB_QWebFrame():new( ... )

FUNCTION QWebFrameFromPointer( ... )
   RETURN HB_QWebFrame():fromPointer( ... )


CREATE CLASS QWebFrame INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QWebFrame

   METHOD  new( ... )

   METHOD  addToJavaScriptWindowObject   // ( cName, oQObject )                                -> NIL
                                         // ( cName, oQObject, nOwn )                          -> NIL
   METHOD  childFrames                   // (  )                                               -> oQList_QWebFrame
   METHOD  contentsSize                  // (  )                                               -> oQSize
   METHOD  frameName                     // (  )                                               -> cQString
   METHOD  geometry                      // (  )                                               -> oQRect
   METHOD  hitTestContent                // ( oQPoint )                                        -> oQWebHitTestResult
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  load                          // ( oQUrl )                                          -> NIL
   METHOD  page                          // (  )                                               -> oQWebPage
   METHOD  parentFrame                   // (  )                                               -> oQWebFrame
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  render                        // ( oQPainter, oQRegion )                            -> NIL
                                         // ( oQPainter )                                      -> NIL
   METHOD  renderTreeDump                // (  )                                               -> cQString
   METHOD  scroll                        // ( nDx, nDy )                                       -> NIL
   METHOD  scrollBarMaximum              // ( nOrientation )                                   -> nInt
   METHOD  scrollBarMinimum              // ( nOrientation )                                   -> nInt
   METHOD  scrollBarPolicy               // ( nOrientation )                                   -> nQt_ScrollBarPolicy
   METHOD  scrollBarValue                // ( nOrientation )                                   -> nInt
   METHOD  scrollPosition                // (  )                                               -> oQPoint
   METHOD  securityOrigin                // (  )                                               -> oQWebSecurityOrigin
   METHOD  setContent                    // ( oQByteArray, cMimeType, oQUrl )                  -> NIL
   METHOD  setHtml                       // ( cHtml, oQUrl )                                   -> NIL
   METHOD  setScrollBarPolicy            // ( nOrientation, nPolicy )                          -> NIL
   METHOD  setScrollBarValue             // ( nOrientation, nValue )                           -> NIL
   METHOD  setScrollPosition             // ( oQPoint )                                        -> NIL
   METHOD  setTextSizeMultiplier         // ( nFactor )                                        -> NIL
   METHOD  setUrl                        // ( oQUrl )                                          -> NIL
   METHOD  setZoomFactor                 // ( nFactor )                                        -> NIL
   METHOD  textSizeMultiplier            // (  )                                               -> nQreal
   METHOD  title                         // (  )                                               -> cQString
   METHOD  toHtml                        // (  )                                               -> cQString
   METHOD  toPlainText                   // (  )                                               -> cQString
   METHOD  url                           // (  )                                               -> oQUrl
   METHOD  zoomFactor                    // (  )                                               -> nQreal
   METHOD  evaluateJavaScript            // ( cScriptSource )                                  -> oQVariant
   METHOD  print                         // ( oQPrinter )                                      -> NIL

   ENDCLASS


METHOD QWebFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebFrame( ... )
   RETURN Self


METHOD QWebFrame:addToJavaScriptWindowObject( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QWebFrame_addToJavaScriptWindowObject_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_addToJavaScriptWindowObject( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:childFrames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWebFrame_childFrames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:contentsSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWebFrame_contentsSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:frameName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_frameName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWebFrame_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:hitTestContent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWebHitTestResultFromPointer( Qt_QWebFrame_hitTestContent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QWebFrame_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:load( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_load( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:page( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebPageFromPointer( Qt_QWebFrame_page( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:parentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebFrameFromPointer( Qt_QWebFrame_parentFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QWebFrame_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:render( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_render_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:renderTreeDump( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_renderTreeDump( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:scroll( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_scroll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:scrollBarMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_scrollBarMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:scrollBarMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_scrollBarMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:scrollBarPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_scrollBarPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:scrollBarValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_scrollBarValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:scrollPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QWebFrame_scrollPosition( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:securityOrigin( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebSecurityOriginFromPointer( Qt_QWebFrame_securityOrigin( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setContent( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QWebFrame_setContent( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_setContent( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_setContent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setHtml( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setScrollBarPolicy( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_setScrollBarPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setScrollBarValue( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebFrame_setScrollBarValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setScrollPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_setScrollPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setTextSizeMultiplier( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_setTextSizeMultiplier( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_setUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:setZoomFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_setZoomFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:textSizeMultiplier( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_textSizeMultiplier( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:toHtml( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_toHtml( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:toPlainText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_toPlainText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:url( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebFrame_url( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:zoomFactor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebFrame_zoomFactor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:evaluateJavaScript( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QWebFrame_evaluateJavaScript( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebFrame:print( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebFrame_print( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

