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


FUNCTION QTextFragment( ... )
   RETURN HB_QTextFragment():new( ... )

FUNCTION QTextFragmentFromPointer( ... )
   RETURN HB_QTextFragment():fromPointer( ... )


CREATE CLASS QTextFragment INHERIT HbQtObjectHandler FUNCTION HB_QTextFragment

   METHOD  new( ... )

   METHOD  charFormat                    // (  )                                               -> oQTextCharFormat
   METHOD  charFormatIndex               // (  )                                               -> nInt
   METHOD  contains                      // ( nPosition )                                      -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  length                        // (  )                                               -> nInt
   METHOD  position                      // (  )                                               -> nInt
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QTextFragment:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextFragment( ... )
   RETURN Self


METHOD QTextFragment:charFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextFragment_charFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFragment:charFormatIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFragment_charFormatIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFragment:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFragment_contains( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFragment:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFragment_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFragment:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFragment_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFragment:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFragment_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFragment:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFragment_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

