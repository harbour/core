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


REQUEST __HBQSCINTILLA


FUNCTION QsciStyledText( ... )
   RETURN HB_QsciStyledText():new( ... )

FUNCTION QsciStyledTextFromPointer( ... )
   RETURN HB_QsciStyledText():fromPointer( ... )


CREATE CLASS QsciStyledText INHERIT HbQtObjectHandler FUNCTION HB_QsciStyledText

   METHOD  new( ... )

   METHOD  text                          // (  )                                               -> cQString
   METHOD  style                         // (  )                                               -> nInt

   ENDCLASS


METHOD QsciStyledText:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciStyledText( ... )
   RETURN Self


METHOD QsciStyledText:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyledText_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyledText:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyledText_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

