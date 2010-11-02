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


FUNCTION QCursor( ... )
   RETURN HB_QCursor():new( ... )

FUNCTION QCursorFromPointer( ... )
   RETURN HB_QCursor():fromPointer( ... )


CREATE CLASS QCursor INHERIT HbQtObjectHandler FUNCTION HB_QCursor

   METHOD  new( ... )

   METHOD  bitmap                        // (  )                                               -> oQBitmap
   METHOD  hotSpot                       // (  )                                               -> oQPoint
   METHOD  mask                          // (  )                                               -> oQBitmap
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  setShape                      // ( nShape )                                         -> NIL
   METHOD  shape                         // (  )                                               -> nQt_CursorShape
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  setPos                        // ( nX, nY )                                         -> NIL
                                         // ( oQPoint )                                        -> NIL

   ENDCLASS


METHOD QCursor:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCursor( ... )
   RETURN Self


METHOD QCursor:bitmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBitmapFromPointer( Qt_QCursor_bitmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:hotSpot( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QCursor_hotSpot( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:mask( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBitmapFromPointer( Qt_QCursor_mask( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QCursor_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:setShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCursor_setShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:shape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCursor_shape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QCursor_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCursor:setPos( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QCursor_setPos( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCursor_setPos_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

