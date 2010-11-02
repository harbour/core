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


FUNCTION QStyleOptionToolBox( ... )
   RETURN HB_QStyleOptionToolBox():new( ... )

FUNCTION QStyleOptionToolBoxFromPointer( ... )
   RETURN HB_QStyleOptionToolBox():fromPointer( ... )


CREATE CLASS QStyleOptionToolBox INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionToolBox

   METHOD  new( ... )

   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QStyleOptionToolBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionToolBox( ... )
   RETURN Self


METHOD QStyleOptionToolBox:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionToolBox_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolBox:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBox_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

