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


FUNCTION QTextCodec( ... )
   RETURN HB_QTextCodec():new( ... )

FUNCTION QTextCodecFromPointer( ... )
   RETURN HB_QTextCodec():fromPointer( ... )


CREATE CLASS QTextCodec INHERIT HbQtObjectHandler FUNCTION HB_QTextCodec

   METHOD  new( ... )

   METHOD  aliases                       // (  )                                               -> oQList_QByteArray>
   METHOD  canEncode                     // ( oQChar )                                         -> lBool
                                         // ( cS )                                             -> lBool
   METHOD  fromUnicode                   // ( cStr )                                           -> oQByteArray
   METHOD  makeDecoder                   // (  )                                               -> oQTextDecoder
   METHOD  makeEncoder                   // (  )                                               -> oQTextEncoder
   METHOD  mibEnum                       // (  )                                               -> nInt
   METHOD  name                          // (  )                                               -> oQByteArray
   METHOD  toUnicode                     // ( oQByteArray )                                    -> cQString
                                         // ( cChars )                                         -> cQString
   METHOD  codecForCStrings              // (  )                                               -> oQTextCodec
   METHOD  codecForHtml                  // ( oQByteArray, oQTextCodec )                       -> oQTextCodec
                                         // ( oQByteArray )                                    -> oQTextCodec
   METHOD  codecForLocale                // (  )                                               -> oQTextCodec
   METHOD  codecForMib                   // ( nMib )                                           -> oQTextCodec
   METHOD  codecForName                  // ( oQByteArray )                                    -> oQTextCodec
                                         // ( cName )                                          -> oQTextCodec
   METHOD  codecForTr                    // (  )                                               -> oQTextCodec
   METHOD  setCodecForCStrings           // ( oQTextCodec )                                    -> NIL
   METHOD  setCodecForLocale             // ( oQTextCodec )                                    -> NIL
   METHOD  setCodecForTr                 // ( oQTextCodec )                                    -> NIL

   ENDCLASS


METHOD QTextCodec:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCodec( ... )
   RETURN Self


METHOD QTextCodec:aliases( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTextCodec_aliases( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:canEncode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_canEncode_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_canEncode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:fromUnicode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QTextCodec_fromUnicode( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:makeDecoder( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDecoderFromPointer( Qt_QTextCodec_makeDecoder( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:makeEncoder( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextEncoderFromPointer( Qt_QTextCodec_makeEncoder( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:mibEnum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCodec_mibEnum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QTextCodec_name( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:toUnicode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_toUnicode_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_toUnicode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:codecForCStrings( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForCStrings( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:codecForHtml( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForHtml( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForHtml_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:codecForLocale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForLocale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:codecForMib( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForMib( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:codecForName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForName_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForName( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:codecForTr( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForTr( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:setCodecForCStrings( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_setCodecForCStrings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:setCodecForLocale( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_setCodecForLocale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCodec:setCodecForTr( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_setCodecForTr( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

