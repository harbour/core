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


FUNCTION QStyleOptionDockWidget( ... )
   RETURN HB_QStyleOptionDockWidget():new( ... )

FUNCTION QStyleOptionDockWidgetFromPointer( ... )
   RETURN HB_QStyleOptionDockWidget():fromPointer( ... )


CREATE CLASS QStyleOptionDockWidget INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionDockWidget

   METHOD  new( ... )

   METHOD  closable                      // (  )                                               -> lBool
   METHOD  floatable                     // (  )                                               -> lBool
   METHOD  movable                       // (  )                                               -> lBool
   METHOD  title                         // (  )                                               -> cQString

   ENDCLASS


METHOD QStyleOptionDockWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionDockWidget( ... )
   RETURN Self


METHOD QStyleOptionDockWidget:closable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionDockWidget_closable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionDockWidget:floatable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionDockWidget_floatable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionDockWidget:movable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionDockWidget_movable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionDockWidget:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionDockWidget_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

