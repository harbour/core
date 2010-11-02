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


REQUEST __HBQTCORE


FUNCTION QTextEncoder( ... )
   RETURN HB_QTextEncoder():new( ... )

FUNCTION QTextEncoderFromPointer( ... )
   RETURN HB_QTextEncoder():fromPointer( ... )


CREATE CLASS QTextEncoder INHERIT HbQtObjectHandler FUNCTION HB_QTextEncoder

   METHOD  new( ... )

   METHOD  fromUnicode                   // ( cStr )                                           -> oQByteArray
                                         // ( oQChar, nLen )                                   -> oQByteArray

   ENDCLASS


METHOD QTextEncoder:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextEncoder( ... )
   RETURN Self


METHOD QTextEncoder:fromUnicode( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QByteArrayFromPointer( Qt_QTextEncoder_fromUnicode_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QTextEncoder_fromUnicode( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

