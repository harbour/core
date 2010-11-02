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


FUNCTION QStyleOptionToolBar( ... )
   RETURN HB_QStyleOptionToolBar():new( ... )

FUNCTION QStyleOptionToolBarFromPointer( ... )
   RETURN HB_QStyleOptionToolBar():fromPointer( ... )


CREATE CLASS QStyleOptionToolBar INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionToolBar

   METHOD  new( ... )

   METHOD  features                      // (  )                                               -> nToolBarFeatures
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  midLineWidth                  // (  )                                               -> nInt
   METHOD  positionOfLine                // (  )                                               -> nToolBarPosition
   METHOD  positionWithinLine            // (  )                                               -> nToolBarPosition
   METHOD  toolBarArea                   // (  )                                               -> nQt_ToolBarArea

   ENDCLASS


METHOD QStyleOptionToolBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionToolBar( ... )
   RETURN Self


METHOD QStyleOptionToolBar:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBar_features( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolBar:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBar_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolBar:midLineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBar_midLineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolBar:positionOfLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBar_positionOfLine( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolBar:positionWithinLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBar_positionWithinLine( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolBar:toolBarArea( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolBar_toolBarArea( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

