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


FUNCTION QSizeGrip( ... )
   RETURN HB_QSizeGrip():new( ... )

FUNCTION QSizeGripFromPointer( ... )
   RETURN HB_QSizeGrip():fromPointer( ... )


CREATE CLASS QSizeGrip INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QSizeGrip

   METHOD  new( ... )


   ENDCLASS


METHOD QSizeGrip:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSizeGrip( ... )
   RETURN Self

