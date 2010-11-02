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


FUNCTION QAbstractScrollArea( ... )
   RETURN HB_QAbstractScrollArea():new( ... )

FUNCTION QAbstractScrollAreaFromPointer( ... )
   RETURN HB_QAbstractScrollArea():fromPointer( ... )


CREATE CLASS QAbstractScrollArea INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QAbstractScrollArea

   METHOD  new( ... )

   METHOD  addScrollBarWidget            // ( oQWidget, nAlignment )                           -> NIL
   METHOD  cornerWidget                  // (  )                                               -> oQWidget
   METHOD  horizontalScrollBar           // (  )                                               -> oQScrollBar
   METHOD  horizontalScrollBarPolicy     // (  )                                               -> nQt_ScrollBarPolicy
   METHOD  maximumViewportSize           // (  )                                               -> oQSize
   METHOD  setCornerWidget               // ( oQWidget )                                       -> NIL
   METHOD  setHorizontalScrollBar        // ( oQScrollBar )                                    -> NIL
   METHOD  setHorizontalScrollBarPolicy  // ( nQt::ScrollBarPolicy )                           -> NIL
   METHOD  setVerticalScrollBar          // ( oQScrollBar )                                    -> NIL
   METHOD  setVerticalScrollBarPolicy    // ( nQt::ScrollBarPolicy )                           -> NIL
   METHOD  setViewport                   // ( oQWidget )                                       -> NIL
   METHOD  verticalScrollBar             // (  )                                               -> oQScrollBar
   METHOD  verticalScrollBarPolicy       // (  )                                               -> nQt_ScrollBarPolicy
   METHOD  viewport                      // (  )                                               -> oQWidget

   ENDCLASS


METHOD QAbstractScrollArea:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractScrollArea( ... )
   RETURN Self


METHOD QAbstractScrollArea:addScrollBarWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractScrollArea_addScrollBarWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:cornerWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QAbstractScrollArea_cornerWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:horizontalScrollBar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QScrollBarFromPointer( Qt_QAbstractScrollArea_horizontalScrollBar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:horizontalScrollBarPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractScrollArea_horizontalScrollBarPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:maximumViewportSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QAbstractScrollArea_maximumViewportSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:setCornerWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractScrollArea_setCornerWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:setHorizontalScrollBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractScrollArea_setHorizontalScrollBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:setHorizontalScrollBarPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractScrollArea_setHorizontalScrollBarPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:setVerticalScrollBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractScrollArea_setVerticalScrollBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:setVerticalScrollBarPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractScrollArea_setVerticalScrollBarPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:setViewport( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractScrollArea_setViewport( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:verticalScrollBar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QScrollBarFromPointer( Qt_QAbstractScrollArea_verticalScrollBar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:verticalScrollBarPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractScrollArea_verticalScrollBarPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractScrollArea:viewport( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QAbstractScrollArea_viewport( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

