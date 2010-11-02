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


FUNCTION QScrollBar( ... )
   RETURN HB_QScrollBar():new( ... )

FUNCTION QScrollBarFromPointer( ... )
   RETURN HB_QScrollBar():fromPointer( ... )


CREATE CLASS QScrollBar INHERIT HbQtObjectHandler, HB_QAbstractSlider FUNCTION HB_QScrollBar

   METHOD  new( ... )


   ENDCLASS


METHOD QScrollBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QScrollBar( ... )
   RETURN Self

