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


FUNCTION QDockWidget( ... )
   RETURN HB_QDockWidget():new( ... )

FUNCTION QDockWidgetFromPointer( ... )
   RETURN HB_QDockWidget():fromPointer( ... )


CREATE CLASS QDockWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDockWidget

   METHOD  new( ... )

   METHOD  allowedAreas                  // (  )                                               -> nQt_DockWidgetAreas
   METHOD  features                      // (  )                                               -> nDockWidgetFeatures
   METHOD  isAreaAllowed                 // ( nArea )                                          -> lBool
   METHOD  isFloating                    // (  )                                               -> lBool
   METHOD  setAllowedAreas               // ( nAreas )                                         -> NIL
   METHOD  setFeatures                   // ( nFeatures )                                      -> NIL
   METHOD  setFloating                   // ( lFloating )                                      -> NIL
   METHOD  setTitleBarWidget             // ( oQWidget )                                       -> NIL
   METHOD  setWidget                     // ( oQWidget )                                       -> NIL
   METHOD  titleBarWidget                // (  )                                               -> oQWidget
   METHOD  toggleViewAction              // (  )                                               -> oQAction
   METHOD  widget                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QDockWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDockWidget( ... )
   RETURN Self


METHOD QDockWidget:allowedAreas( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDockWidget_allowedAreas( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDockWidget_features( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:isAreaAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDockWidget_isAreaAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:isFloating( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDockWidget_isFloating( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:setAllowedAreas( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDockWidget_setAllowedAreas( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:setFeatures( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDockWidget_setFeatures( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:setFloating( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDockWidget_setFloating( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:setTitleBarWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDockWidget_setTitleBarWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:setWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDockWidget_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:titleBarWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDockWidget_titleBarWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:toggleViewAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDockWidget_toggleViewAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDockWidget:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDockWidget_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

