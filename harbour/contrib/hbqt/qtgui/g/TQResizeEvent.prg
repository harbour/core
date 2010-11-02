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


FUNCTION QResizeEvent( ... )
   RETURN HB_QResizeEvent():new( ... )

FUNCTION QResizeEventFromPointer( ... )
   RETURN HB_QResizeEvent():fromPointer( ... )


CREATE CLASS QResizeEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QResizeEvent

   METHOD  new( ... )

   METHOD  oldSize                       // (  )                                               -> oQSize
   METHOD  size                          // (  )                                               -> oQSize

   ENDCLASS


METHOD QResizeEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QResizeEvent( ... )
   RETURN Self


METHOD QResizeEvent:oldSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QResizeEvent_oldSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QResizeEvent:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QResizeEvent_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

