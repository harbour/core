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


FUNCTION QFocusFrame( ... )
   RETURN HB_QFocusFrame():new( ... )

FUNCTION QFocusFrameFromPointer( ... )
   RETURN HB_QFocusFrame():fromPointer( ... )


CREATE CLASS QFocusFrame INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QFocusFrame

   METHOD  new( ... )

   METHOD  setWidget                     // ( oQWidget )                                       -> NIL
   METHOD  widget                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QFocusFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFocusFrame( ... )
   RETURN Self


METHOD QFocusFrame:setWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFocusFrame_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFocusFrame:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QFocusFrame_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

