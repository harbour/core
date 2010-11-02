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


FUNCTION QWindowsStyle( ... )
   RETURN HB_QWindowsStyle():new( ... )

FUNCTION QWindowsStyleFromPointer( ... )
   RETURN HB_QWindowsStyle():fromPointer( ... )


CREATE CLASS QWindowsStyle INHERIT HbQtObjectHandler, HB_QCommonStyle FUNCTION HB_QWindowsStyle

   METHOD  new( ... )


   ENDCLASS


METHOD QWindowsStyle:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWindowsStyle( ... )
   RETURN Self

