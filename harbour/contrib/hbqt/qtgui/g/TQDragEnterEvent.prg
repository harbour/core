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


FUNCTION QDragEnterEvent( ... )
   RETURN HB_QDragEnterEvent():new( ... )

FUNCTION QDragEnterEventFromPointer( ... )
   RETURN HB_QDragEnterEvent():fromPointer( ... )


CREATE CLASS QDragEnterEvent INHERIT HbQtObjectHandler, HB_QDragMoveEvent FUNCTION HB_QDragEnterEvent

   METHOD  new( ... )


   ENDCLASS


METHOD QDragEnterEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDragEnterEvent( ... )
   RETURN Self

