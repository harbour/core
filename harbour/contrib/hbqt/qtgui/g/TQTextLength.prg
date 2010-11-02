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


FUNCTION QTextLength( ... )
   RETURN HB_QTextLength():new( ... )

FUNCTION QTextLengthFromPointer( ... )
   RETURN HB_QTextLength():fromPointer( ... )


CREATE CLASS QTextLength INHERIT HbQtObjectHandler FUNCTION HB_QTextLength

   METHOD  new( ... )

   METHOD  rawValue                      // (  )                                               -> nQreal
   METHOD  type                          // (  )                                               -> nType
   METHOD  value                         // ( nMaximumLength )                                 -> nQreal

   ENDCLASS


METHOD QTextLength:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextLength( ... )
   RETURN Self


METHOD QTextLength:rawValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLength_rawValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLength:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLength_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLength:value( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLength_value( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

