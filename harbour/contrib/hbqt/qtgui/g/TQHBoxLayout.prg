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


FUNCTION QHBoxLayout( ... )
   RETURN HB_QHBoxLayout():new( ... )

FUNCTION QHBoxLayoutFromPointer( ... )
   RETURN HB_QHBoxLayout():fromPointer( ... )


CREATE CLASS QHBoxLayout INHERIT HbQtObjectHandler, HB_QBoxLayout FUNCTION HB_QHBoxLayout

   METHOD  new( ... )


   ENDCLASS


METHOD QHBoxLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHBoxLayout( ... )
   RETURN Self

