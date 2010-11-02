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


FUNCTION QStackedWidget( ... )
   RETURN HB_QStackedWidget():new( ... )

FUNCTION QStackedWidgetFromPointer( ... )
   RETURN HB_QStackedWidget():fromPointer( ... )


CREATE CLASS QStackedWidget INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QStackedWidget

   METHOD  new( ... )

   METHOD  addWidget                     // ( oQWidget )                                       -> nInt
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  currentWidget                 // (  )                                               -> oQWidget
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  insertWidget                  // ( nIndex, oQWidget )                               -> nInt
   METHOD  removeWidget                  // ( oQWidget )                                       -> NIL
   METHOD  widget                        // ( nIndex )                                         -> oQWidget
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL
   METHOD  setCurrentWidget              // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QStackedWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStackedWidget( ... )
   RETURN Self


METHOD QStackedWidget:addWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStackedWidget_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStackedWidget_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStackedWidget_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:currentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QStackedWidget_currentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStackedWidget_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:insertWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStackedWidget_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:removeWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStackedWidget_removeWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:widget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QStackedWidget_widget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStackedWidget_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStackedWidget:setCurrentWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStackedWidget_setCurrentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

