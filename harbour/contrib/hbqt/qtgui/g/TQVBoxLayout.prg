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


FUNCTION QVBoxLayout( ... )
   RETURN HB_QVBoxLayout():new( ... )

FUNCTION QVBoxLayoutFromPointer( ... )
   RETURN HB_QVBoxLayout():fromPointer( ... )


CREATE CLASS QVBoxLayout INHERIT HbQtObjectHandler, HB_QBoxLayout FUNCTION HB_QVBoxLayout

   METHOD  new( ... )


   ENDCLASS


METHOD QVBoxLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QVBoxLayout( ... )
   RETURN Self

