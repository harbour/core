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


REQUEST __HBQTGUI


FUNCTION QAbstractTextDocumentLayout( ... )
   RETURN HB_QAbstractTextDocumentLayout():new( ... )

FUNCTION QAbstractTextDocumentLayoutFromPointer( ... )
   RETURN HB_QAbstractTextDocumentLayout():fromPointer( ... )


CREATE CLASS QAbstractTextDocumentLayout INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QAbstractTextDocumentLayout

   METHOD  new( ... )

   METHOD  anchorAt                      // ( oQPointF )                                       -> cQString
   METHOD  blockBoundingRect             // ( oQTextBlock )                                    -> oQRectF
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  documentSize                  // (  )                                               -> oQSizeF
   METHOD  frameBoundingRect             // ( oQTextFrame )                                    -> oQRectF
   METHOD  hitTest                       // ( oQPointF, nAccuracy )                            -> nInt
   METHOD  pageCount                     // (  )                                               -> nInt
   METHOD  paintDevice                   // (  )                                               -> oQPaintDevice
   METHOD  registerHandler               // ( nObjectType, oQObject )                          -> NIL
   METHOD  setPaintDevice                // ( oQPaintDevice )                                  -> NIL

   ENDCLASS


METHOD QAbstractTextDocumentLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractTextDocumentLayout( ... )
   RETURN Self


METHOD QAbstractTextDocumentLayout:anchorAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractTextDocumentLayout_anchorAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:blockBoundingRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QAbstractTextDocumentLayout_blockBoundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QAbstractTextDocumentLayout_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:documentSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QAbstractTextDocumentLayout_documentSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:frameBoundingRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QAbstractTextDocumentLayout_frameBoundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:hitTest( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractTextDocumentLayout_hitTest( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:pageCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractTextDocumentLayout_pageCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:paintDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintDeviceFromPointer( Qt_QAbstractTextDocumentLayout_paintDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:registerHandler( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractTextDocumentLayout_registerHandler( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractTextDocumentLayout:setPaintDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractTextDocumentLayout_setPaintDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

