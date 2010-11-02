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


FUNCTION QWidgetItem( ... )
   RETURN HB_QWidgetItem():new( ... )

FUNCTION QWidgetItemFromPointer( ... )
   RETURN HB_QWidgetItem():fromPointer( ... )


CREATE CLASS QWidgetItem INHERIT HbQtObjectHandler, HB_QLayoutItem FUNCTION HB_QWidgetItem

   METHOD  new( ... )

   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  widget                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWidgetItem( ... )
   RETURN Self


METHOD QWidgetItem:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidgetItem_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWidgetItem:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidgetItem_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

