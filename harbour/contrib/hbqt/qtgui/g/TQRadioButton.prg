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


FUNCTION QRadioButton( ... )
   RETURN HB_QRadioButton():new( ... )

FUNCTION QRadioButtonFromPointer( ... )
   RETURN HB_QRadioButton():fromPointer( ... )


CREATE CLASS QRadioButton INHERIT HbQtObjectHandler, HB_QAbstractButton FUNCTION HB_QRadioButton

   METHOD  new( ... )


   ENDCLASS


METHOD QRadioButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRadioButton( ... )
   RETURN Self

