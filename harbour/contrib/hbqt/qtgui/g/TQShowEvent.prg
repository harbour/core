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


FUNCTION QShowEvent( ... )
   RETURN HB_QShowEvent():new( ... )

FUNCTION QShowEventFromPointer( ... )
   RETURN HB_QShowEvent():fromPointer( ... )


CREATE CLASS QShowEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QShowEvent

   METHOD  new( ... )


   ENDCLASS


METHOD QShowEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QShowEvent( ... )
   RETURN Self

