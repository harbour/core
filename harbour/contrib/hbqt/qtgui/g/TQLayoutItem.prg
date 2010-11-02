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


FUNCTION QLayoutItem( ... )
   RETURN HB_QLayoutItem():new( ... )

FUNCTION QLayoutItemFromPointer( ... )
   RETURN HB_QLayoutItem():fromPointer( ... )


CREATE CLASS QLayoutItem INHERIT HbQtObjectHandler FUNCTION HB_QLayoutItem

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  controlTypes                  // (  )                                               -> nQSizePolicy_ControlTypes
   METHOD  expandingDirections           // (  )                                               -> nQt_Orientations
   METHOD  geometry                      // (  )                                               -> oQRect
   METHOD  hasHeightForWidth             // (  )                                               -> lBool
   METHOD  heightForWidth                // ( nW )                                             -> nInt
   METHOD  invalidate                    // (  )                                               -> NIL
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  layout                        // (  )                                               -> oQLayout
   METHOD  maximumSize                   // (  )                                               -> oQSize
   METHOD  minimumHeightForWidth         // ( nW )                                             -> nInt
   METHOD  minimumSize                   // (  )                                               -> oQSize
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setGeometry                   // ( oQRect )                                         -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  spacerItem                    // (  )                                               -> oQSpacerItem
   METHOD  widget                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QLayoutItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLayoutItem( ... )
   RETURN Self


METHOD QLayoutItem:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayoutItem_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:controlTypes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayoutItem_controlTypes( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:expandingDirections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayoutItem_expandingDirections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QLayoutItem_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:hasHeightForWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayoutItem_hasHeightForWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:heightForWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayoutItem_heightForWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:invalidate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayoutItem_invalidate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayoutItem_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:layout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLayoutFromPointer( Qt_QLayoutItem_layout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:maximumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayoutItem_maximumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:minimumHeightForWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayoutItem_minimumHeightForWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:minimumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayoutItem_minimumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayoutItem_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:setGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayoutItem_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayoutItem_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:spacerItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSpacerItemFromPointer( Qt_QLayoutItem_spacerItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayoutItem:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QLayoutItem_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

