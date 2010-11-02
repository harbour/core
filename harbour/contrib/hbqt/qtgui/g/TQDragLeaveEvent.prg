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


FUNCTION QDragLeaveEvent( ... )
   RETURN HB_QDragLeaveEvent():new( ... )

FUNCTION QDragLeaveEventFromPointer( ... )
   RETURN HB_QDragLeaveEvent():fromPointer( ... )


CREATE CLASS QDragLeaveEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QDragLeaveEvent

   METHOD  new( ... )


   ENDCLASS


METHOD QDragLeaveEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDragLeaveEvent( ... )
   RETURN Self

