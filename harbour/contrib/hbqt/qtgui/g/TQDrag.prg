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


FUNCTION QDrag( ... )
   RETURN HB_QDrag():new( ... )

FUNCTION QDragFromPointer( ... )
   RETURN HB_QDrag():fromPointer( ... )


CREATE CLASS QDrag INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QDrag

   METHOD  new( ... )

   METHOD  exec                          // ( nSupportedActions )                              -> nQt_DropAction
                                         // ( nSupportedActions, nDefaultDropAction )          -> nQt_DropAction
   METHOD  hotSpot                       // (  )                                               -> oQPoint
   METHOD  mimeData                      // (  )                                               -> oQMimeData
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  setDragCursor                 // ( oQPixmap, nAction )                              -> NIL
   METHOD  setHotSpot                    // ( oQPoint )                                        -> NIL
   METHOD  setMimeData                   // ( oQMimeData )                                     -> NIL
   METHOD  setPixmap                     // ( oQPixmap )                                       -> NIL
   METHOD  source                        // (  )                                               -> oQWidget
   METHOD  target                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QDrag:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDrag( ... )
   RETURN Self


METHOD QDrag:exec( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDrag_exec_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDrag_exec( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDrag_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:hotSpot( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QDrag_hotSpot( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:mimeData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMimeDataFromPointer( Qt_QDrag_mimeData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QDrag_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:setDragCursor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDrag_setDragCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:setHotSpot( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDrag_setHotSpot( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:setMimeData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDrag_setMimeData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:setPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDrag_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:source( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDrag_source( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDrag:target( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDrag_target( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

