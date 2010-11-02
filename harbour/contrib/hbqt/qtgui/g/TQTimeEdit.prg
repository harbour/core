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


FUNCTION QTimeEdit( ... )
   RETURN HB_QTimeEdit():new( ... )

FUNCTION QTimeEditFromPointer( ... )
   RETURN HB_QTimeEdit():fromPointer( ... )


CREATE CLASS QTimeEdit INHERIT HbQtObjectHandler, HB_QDateTimeEdit FUNCTION HB_QTimeEdit

   METHOD  new( ... )


   ENDCLASS


METHOD QTimeEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTimeEdit( ... )
   RETURN Self

