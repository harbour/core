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


FUNCTION QToolBox( ... )
   RETURN HB_QToolBox():new( ... )

FUNCTION QToolBoxFromPointer( ... )
   RETURN HB_QToolBox():fromPointer( ... )


CREATE CLASS QToolBox INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QToolBox

   METHOD  new( ... )

   METHOD  addItem                       // ( oQWidget, coQIcon, cText )                       -> nInt
                                         // ( oQWidget, cText )                                -> nInt
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  currentWidget                 // (  )                                               -> oQWidget
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  insertItem                    // ( nIndex, oQWidget, coQIcon, cText )               -> nInt
                                         // ( nIndex, oQWidget, cText )                        -> nInt
   METHOD  isItemEnabled                 // ( nIndex )                                         -> lBool
   METHOD  itemIcon                      // ( nIndex )                                         -> oQIcon
   METHOD  itemText                      // ( nIndex )                                         -> cQString
   METHOD  itemToolTip                   // ( nIndex )                                         -> cQString
   METHOD  removeItem                    // ( nIndex )                                         -> NIL
   METHOD  setItemEnabled                // ( nIndex, lEnabled )                               -> NIL
   METHOD  setItemIcon                   // ( nIndex, coQIcon )                                -> NIL
   METHOD  setItemText                   // ( nIndex, cText )                                  -> NIL
   METHOD  setItemToolTip                // ( nIndex, cToolTip )                               -> NIL
   METHOD  widget                        // ( nIndex )                                         -> oQWidget
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL
   METHOD  setCurrentWidget              // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QToolBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolBox( ... )
   RETURN Self


METHOD QToolBox:addItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QToolBox_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QToolBox_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBox_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBox_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:currentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QToolBox_currentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:insertItem( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. ( hb_isObject( hb_pvalue( 3 ) ) .OR. hb_isChar( hb_pvalue( 3 ) ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN Qt_QToolBox_insertItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QToolBox_insertItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:isItemEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_isItemEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:itemIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QToolBox_itemIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:itemText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_itemText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:itemToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_itemToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:setItemEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QToolBox_setItemEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:setItemIcon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) )
         RETURN Qt_QToolBox_setItemIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:setItemText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QToolBox_setItemText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:setItemToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QToolBox_setItemToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:widget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QToolBox_widget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBox:setCurrentWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBox_setCurrentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

