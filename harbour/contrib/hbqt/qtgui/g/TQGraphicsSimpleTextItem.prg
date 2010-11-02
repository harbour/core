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


FUNCTION QGraphicsSimpleTextItem( ... )
   RETURN HB_QGraphicsSimpleTextItem():new( ... )

FUNCTION QGraphicsSimpleTextItemFromPointer( ... )
   RETURN HB_QGraphicsSimpleTextItem():fromPointer( ... )


CREATE CLASS QGraphicsSimpleTextItem INHERIT HbQtObjectHandler, HB_QAbstractGraphicsShapeItem FUNCTION HB_QGraphicsSimpleTextItem

   METHOD  new( ... )

   METHOD  font                          // (  )                                               -> oQFont
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QGraphicsSimpleTextItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSimpleTextItem( ... )
   RETURN Self


METHOD QGraphicsSimpleTextItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QGraphicsSimpleTextItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSimpleTextItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsSimpleTextItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSimpleTextItem:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsSimpleTextItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSimpleTextItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSimpleTextItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

