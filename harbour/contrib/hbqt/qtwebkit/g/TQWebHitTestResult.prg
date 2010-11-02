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


FUNCTION QWebHitTestResult( ... )
   RETURN HB_QWebHitTestResult():new( ... )

FUNCTION QWebHitTestResultFromPointer( ... )
   RETURN HB_QWebHitTestResult():fromPointer( ... )


CREATE CLASS QWebHitTestResult INHERIT HbQtObjectHandler FUNCTION HB_QWebHitTestResult

   METHOD  new( ... )

   METHOD  alternateText                 // (  )                                               -> cQString
   METHOD  boundingRect                  // (  )                                               -> oQRect
   METHOD  frame                         // (  )                                               -> oQWebFrame
   METHOD  imageUrl                      // (  )                                               -> oQUrl
   METHOD  isContentEditable             // (  )                                               -> lBool
   METHOD  isContentSelected             // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  linkTargetFrame               // (  )                                               -> oQWebFrame
   METHOD  linkText                      // (  )                                               -> cQString
   METHOD  linkTitle                     // (  )                                               -> oQUrl
   METHOD  linkUrl                       // (  )                                               -> oQUrl
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  title                         // (  )                                               -> cQString

   ENDCLASS


METHOD QWebHitTestResult:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebHitTestResult( ... )
   RETURN Self


METHOD QWebHitTestResult:alternateText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHitTestResult_alternateText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWebHitTestResult_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:frame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebFrameFromPointer( Qt_QWebHitTestResult_frame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:imageUrl( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebHitTestResult_imageUrl( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:isContentEditable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHitTestResult_isContentEditable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:isContentSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHitTestResult_isContentSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHitTestResult_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:linkTargetFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebFrameFromPointer( Qt_QWebHitTestResult_linkTargetFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:linkText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHitTestResult_linkText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:linkTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebHitTestResult_linkTitle( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:linkUrl( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebHitTestResult_linkUrl( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QWebHitTestResult_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QWebHitTestResult_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHitTestResult:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHitTestResult_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

