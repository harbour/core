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


FUNCTION QGraphicsLinearLayout( ... )
   RETURN HB_QGraphicsLinearLayout():new( ... )

FUNCTION QGraphicsLinearLayoutFromPointer( ... )
   RETURN HB_QGraphicsLinearLayout():fromPointer( ... )


CREATE CLASS QGraphicsLinearLayout INHERIT HbQtObjectHandler, HB_QGraphicsLayout FUNCTION HB_QGraphicsLinearLayout

   METHOD  new( ... )

   METHOD  addItem                       // ( oQGraphicsLayoutItem )                           -> NIL
   METHOD  addStretch                    // ( nStretch )                                       -> NIL
   METHOD  alignment                     // ( oQGraphicsLayoutItem )                           -> nQt_Alignment
   METHOD  insertItem                    // ( nIndex, oQGraphicsLayoutItem )                   -> NIL
   METHOD  insertStretch                 // ( nIndex, nStretch )                               -> NIL
   METHOD  itemSpacing                   // ( nIndex )                                         -> nQreal
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  removeAt                      // ( nIndex )                                         -> NIL
   METHOD  removeItem                    // ( oQGraphicsLayoutItem )                           -> NIL
   METHOD  setAlignment                  // ( oQGraphicsLayoutItem, nAlignment )               -> NIL
   METHOD  setItemSpacing                // ( nIndex, nSpacing )                               -> NIL
   METHOD  setOrientation                // ( nOrientation )                                   -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setStretchFactor              // ( oQGraphicsLayoutItem, nStretch )                 -> NIL
   METHOD  spacing                       // (  )                                               -> nQreal
   METHOD  stretchFactor                 // ( oQGraphicsLayoutItem )                           -> nInt

   ENDCLASS


METHOD QGraphicsLinearLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsLinearLayout( ... )
   RETURN Self


METHOD QGraphicsLinearLayout:addItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:addStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_addStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsLinearLayout_addStretch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:alignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_alignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:insertItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLinearLayout_insertItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:insertStretch( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLinearLayout_insertStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_insertStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:itemSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_itemSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLinearLayout_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:removeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_removeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:setAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLinearLayout_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:setItemSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLinearLayout_setItemSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:setStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLinearLayout_setStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLinearLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLinearLayout:stretchFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLinearLayout_stretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

