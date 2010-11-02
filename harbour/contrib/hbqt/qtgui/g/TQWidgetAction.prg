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


FUNCTION QWidgetAction( ... )
   RETURN HB_QWidgetAction():new( ... )

FUNCTION QWidgetActionFromPointer( ... )
   RETURN HB_QWidgetAction():fromPointer( ... )


CREATE CLASS QWidgetAction INHERIT HbQtObjectHandler, HB_QAction FUNCTION HB_QWidgetAction

   METHOD  new( ... )

   METHOD  defaultWidget                 // (  )                                               -> oQWidget
   METHOD  releaseWidget                 // ( oQWidget )                                       -> NIL
   METHOD  requestWidget                 // ( oQWidget )                                       -> oQWidget
   METHOD  setDefaultWidget              // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QWidgetAction:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWidgetAction( ... )
   RETURN Self


METHOD QWidgetAction:defaultWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidgetAction_defaultWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWidgetAction:releaseWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidgetAction_releaseWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWidgetAction:requestWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QWidgetAction_requestWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWidgetAction:setDefaultWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidgetAction_setDefaultWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

