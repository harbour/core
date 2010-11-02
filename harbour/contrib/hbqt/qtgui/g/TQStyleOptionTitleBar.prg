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


FUNCTION QStyleOptionTitleBar( ... )
   RETURN HB_QStyleOptionTitleBar():new( ... )

FUNCTION QStyleOptionTitleBarFromPointer( ... )
   RETURN HB_QStyleOptionTitleBar():fromPointer( ... )


CREATE CLASS QStyleOptionTitleBar INHERIT HbQtObjectHandler, HB_QStyleOptionComplex FUNCTION HB_QStyleOptionTitleBar

   METHOD  new( ... )

   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  text                          // (  )                                               -> cQString
   METHOD  titleBarFlags                 // (  )                                               -> nQt_WindowFlags
   METHOD  titleBarState                 // (  )                                               -> nInt

   ENDCLASS


METHOD QStyleOptionTitleBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionTitleBar( ... )
   RETURN Self


METHOD QStyleOptionTitleBar:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionTitleBar_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTitleBar:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTitleBar_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTitleBar:titleBarFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTitleBar_titleBarFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTitleBar:titleBarState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTitleBar_titleBarState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

