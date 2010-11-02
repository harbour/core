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


FUNCTION QLayout( ... )
   RETURN HB_QLayout():new( ... )

FUNCTION QLayoutFromPointer( ... )
   RETURN HB_QLayout():fromPointer( ... )


CREATE CLASS QLayout INHERIT HbQtObjectHandler, HB_QObject, HB_QLayoutItem FUNCTION HB_QLayout

   METHOD  new( ... )

   METHOD  activate                      // (  )                                               -> lBool
   METHOD  addItem                       // ( oQLayoutItem )                                   -> NIL
   METHOD  addWidget                     // ( oQWidget )                                       -> NIL
   METHOD  contentsRect                  // (  )                                               -> oQRect
   METHOD  count                         // (  )                                               -> nInt
   METHOD  expandingDirections           // (  )                                               -> nQt_Orientations
   METHOD  getContentsMargins            // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  isEnabled                     // (  )                                               -> lBool
   METHOD  itemAt                        // ( nIndex )                                         -> oQLayoutItem
   METHOD  maximumSize                   // (  )                                               -> oQSize
   METHOD  menuBar                       // (  )                                               -> oQWidget
   METHOD  minimumSize                   // (  )                                               -> oQSize
   METHOD  parentWidget                  // (  )                                               -> oQWidget
   METHOD  removeItem                    // ( oQLayoutItem )                                   -> NIL
   METHOD  removeWidget                  // ( oQWidget )                                       -> NIL
   METHOD  setAlignment                  // ( oQWidget, nAlignment )                           -> lBool
                                         // ( nAlignment )                                     -> NIL
                                         // ( oQLayout, nAlignment )                           -> lBool
   METHOD  setContentsMargins            // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setEnabled                    // ( lEnable )                                        -> NIL
   METHOD  setMenuBar                    // ( oQWidget )                                       -> NIL
   METHOD  setSizeConstraint             // ( nSizeConstraint )                                -> NIL
   METHOD  setSpacing                    // ( nInt )                                           -> NIL
   METHOD  sizeConstraint                // (  )                                               -> nSizeConstraint
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  takeAt                        // ( nIndex )                                         -> oQLayoutItem
   METHOD  update                        // (  )                                               -> NIL
   METHOD  closestAcceptableSize         // ( oQWidget, oQSize )                               -> oQSize

   ENDCLASS


METHOD QLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLayout( ... )
   RETURN Self


METHOD QLayout:activate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_activate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:addItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:addWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:contentsRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QLayout_contentsRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:expandingDirections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_expandingDirections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:getContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLayout_getContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:isEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_isEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:itemAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QLayoutItemFromPointer( Qt_QLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:maximumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayout_maximumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:menuBar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QLayout_menuBar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:minimumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayout_minimumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:parentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QLayout_parentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:removeWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_removeWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QLAYOUT"
            RETURN Qt_QLayout_setAlignment_2( ::pPtr, ... )
         CASE "QWIDGET"
            RETURN Qt_QLayout_setAlignment( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setAlignment_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLayout_setContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setMenuBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setMenuBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setSizeConstraint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setSizeConstraint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:sizeConstraint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_sizeConstraint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:takeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QLayoutItemFromPointer( Qt_QLayout_takeAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:update( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_update( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:closestAcceptableSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QLayout_closestAcceptableSize( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

