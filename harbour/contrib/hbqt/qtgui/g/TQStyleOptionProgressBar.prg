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


FUNCTION QStyleOptionProgressBar( ... )
   RETURN HB_QStyleOptionProgressBar():new( ... )

FUNCTION QStyleOptionProgressBarFromPointer( ... )
   RETURN HB_QStyleOptionProgressBar():fromPointer( ... )


CREATE CLASS QStyleOptionProgressBar INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionProgressBar

   METHOD  new( ... )

   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  progress                      // (  )                                               -> nInt
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nQt_Alignment
   METHOD  textVisible                   // (  )                                               -> lBool

   ENDCLASS


METHOD QStyleOptionProgressBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionProgressBar( ... )
   RETURN Self


METHOD QStyleOptionProgressBar:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionProgressBar_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionProgressBar:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionProgressBar_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionProgressBar:progress( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionProgressBar_progress( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionProgressBar:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionProgressBar_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionProgressBar:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionProgressBar_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionProgressBar:textVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionProgressBar_textVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

