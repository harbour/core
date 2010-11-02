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


FUNCTION QTextItem( ... )
   RETURN HB_QTextItem():new( ... )

FUNCTION QTextItemFromPointer( ... )
   RETURN HB_QTextItem():fromPointer( ... )


CREATE CLASS QTextItem INHERIT HbQtObjectHandler FUNCTION HB_QTextItem

   METHOD  new( ... )

   METHOD  ascent                        // (  )                                               -> nQreal
   METHOD  descent                       // (  )                                               -> nQreal
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  renderFlags                   // (  )                                               -> nRenderFlags
   METHOD  text                          // (  )                                               -> cQString
   METHOD  width                         // (  )                                               -> nQreal

   ENDCLASS


METHOD QTextItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextItem( ... )
   RETURN Self


METHOD QTextItem:ascent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextItem_ascent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextItem:descent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextItem_descent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTextItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextItem:renderFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextItem_renderFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextItem:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextItem_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

