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


FUNCTION QTextDecoder( ... )
   RETURN HB_QTextDecoder():new( ... )

FUNCTION QTextDecoderFromPointer( ... )
   RETURN HB_QTextDecoder():fromPointer( ... )


CREATE CLASS QTextDecoder INHERIT HbQtObjectHandler FUNCTION HB_QTextDecoder

   METHOD  new( ... )

   METHOD  toUnicode                     // ( cChars, nLen )                                   -> cQString

   ENDCLASS


METHOD QTextDecoder:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextDecoder( ... )
   RETURN Self


METHOD QTextDecoder:toUnicode( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextDecoder_toUnicode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

